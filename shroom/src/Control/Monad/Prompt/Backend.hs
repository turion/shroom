{- | The adapter seam: what an LLM transport must provide for shroom to run a
'Control.Monad.Prompt.Effect.Prompt' program against it.

A 'Backend' only ever has to make one raw chat call: hand over the conversation
so far, a description of the expected result type, its JSON schema, and the
tools currently on offer, and get back either the model's final answer, a
set of tool calls it wants made, or a 'BackendError'. The loop itself —
dispatching each call and feeding the results back in for the next call —
belongs to the caller ("Control.Monad.Prompt.Effect"\'s
'Control.Monad.Prompt.Effect.Prompt' effect, see
'Control.Monad.Prompt.Effect.promptTools'), one call at a time.

A 'Backend' is a plain record of one function, not a typeclass — there is no
instance to declare and so nothing an implementor has to name; constructing a
value /is/ satisfying the interface. That is deliberate: every argument and
result type it mentions is one of shroom's own
('ContextItem', 'ToolCall', 'ToolDef', 'Text', 'Value', 'BackendReply',
'BackendError') or from a package any adapter depends on anyway ('aeson',
'text'), and nothing here forces a second dependency on any particular
transport package. 'schemaWithDefs' is re-exported here too, for the same
reason: an adapter needs it to turn a 'Data.OpenApi.ToSchema' instance into
the JSON schema 'Value' a provider's structured-output call expects.
"Control.Monad.Prompt.Schema" itself stays internal — its other contents
(@normalizeSchemaForStructuredOutput@, @inlineSchema@) are quirk-workarounds
for particular backends' schema formats, not something an adapter has
business calling directly.

Native provider tool-calling survives through this seam too: a call carries
the provider's own call id in 'ToolCall'\'s
@toolCallId@, so an adapter that gets that id back from its provider
(Anthropic's @tool_use_id@) can still link a result to the call that
produced it once the exchange has round-tripped through 'ContextItem'\'s
'ToolCallMessage' \/ 'ToolResultMessage', and a reply may carry more than
one call so parallel tool calls stay expressible.
'Control.Monad.Prompt.FileMock' builds a 'Backend' value, as do the mock
backends in @test-utils@'s @TestUtils@. A package that depends only on
@shroom@ can build one too; @shroom-baikai@ does exactly that, interpreting
@baikai-effectful@\'s @Baikai@ effect into a 'Backend' — that is the worked
example for anyone weighing whether writing a replacement adapter is a
weekend or a rescue.

'ContextItem', 'ToolCall' and 'ToolResult' live in this module — rather than
under "Data.Shroom." — because they name 'Backend', 'BackendReply' and
'BackendError' throughout their own Haddock, and the reverse is equally
true: this module cannot be understood without them. None of the three
depend on @effectful@ or on the program layer, so putting them here does not
cross the pure\/effectful boundary "Data.Shroom.Class" documents; they simply
sit on the adapter seam's side of it rather than in the schema/description
half.
-}
module Control.Monad.Prompt.Backend (
  Backend (..),
  BackendReply (..),
  BackendError (..),
  ToolDef (..),
  ContextItem (..),
  ToolCall (..),
  ToolResult (..),
  renderContextItems,
  schemaWithDefs,
) where

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (Value)

-- shroom
import Control.Monad.Prompt.Schema (ToolDef (..), schemaWithDefs)

-- * Tool calls and results

{- | One tool call a model requested, carried either inside a 'Backend'\'s
reply ('BackendToolCalls') or, once dispatched, replayed into the
conversation as part of a 'ToolCallMessage'.
-}
data ToolCall = ToolCall
  { toolCallId :: Text
  {- ^ The provider's own id for this call. A provider that links a tool
  result back to the call that produced it (e.g. Anthropic's
  @tool_use_id@) needs this echoed back unchanged in the matching
  'ToolResult' — that linking is exactly what widening 'Backend' with a
  tool channel, instead of flattening tool calls to text, was for.
  -}
  , toolCallName :: Text
  , toolCallArguments :: Value
  }
  deriving (Eq, Show)

{- | The outcome of dispatching one 'ToolCall', linked back to it by
'toolResultId'.
-}
data ToolResult = ToolResult
  { toolResultId :: Text
  -- ^ Echoes the originating 'ToolCall'\'s 'toolCallId'.
  , toolResultName :: Text
  , toolResultOutcome :: Either Text Text
  {- ^ 'Left' — the tool failed, with a message for the model. 'Right' —
  the tool's own result text.
  -}
  }
  deriving (Eq, Show)

-- * Context items

{- | A single item in the LLM conversation history.
Backends map these to their native message types (Anthropic: @system@ field
or @user@\/@assistant@ roles; Ollama: @system@\/@user@\/@assistant@ roles).
-}
data ContextItem
  = -- | Instructions or persona, sent before any user turns.
    SystemMessage Text
  | -- | A user turn in the conversation.
    UserMessage Text
  | {- | A previous model response. Appended automatically after each successful
      'Control.Monad.Prompt.Effect.prompt' call so subsequent prompts can
      reference prior outputs naturally.
    -}
    AssistantMessage Text
  | {- | Tool calls a model requested in a single turn (parallel calls travel
      together as one item, not one apiece, so they replay as one assistant
      turn). Appended after a 'Backend' reply carries 'BackendToolCalls',
      alongside the matching 'ToolResultMessage' once the calls have been
      dispatched.
    -}
    ToolCallMessage [ToolCall]
  | {- | The results of dispatching the calls in a 'ToolCallMessage', one
      per call, each still linked back to its call by 'toolResultId'.
    -}
    ToolResultMessage [ToolResult]
  deriving (Eq, Show)

{- | Render a list of 'ContextItem' values to a flat 'Text' for display or
simple backends that do not support structured message histories.
Each item is prefixed with its role and separated by newlines.
-}
renderContextItems :: [ContextItem] -> Text
renderContextItems = T.intercalate "\n" . fmap render
  where
    render (SystemMessage t) = "[system] " <> t
    render (UserMessage t) = t
    render (AssistantMessage t) = "[assistant] " <> t
    render (ToolCallMessage calls) = "[tool call] " <> T.intercalate ", " (fmap toolCallName calls)
    render (ToolResultMessage results) = "[tool result] " <> T.intercalate ", " (fmap toolResultName results)

{- | A chat backend, reduced to the one call every backend must make: given
the conversation so far, the expected result type's description, its JSON
schema and the tools currently on offer, produce the model's reply or an
error.

  * @conversation@ — the accumulated 'ContextItem' history.
  * @typeDescription@ — appended as the final turn; describes the expected
    output type in prose.
  * @schema@ — the JSON schema the response must satisfy.
  * @tools@ — the tool definitions on offer for this call; empty means no
    tools are being offered.

Returns a 'BackendReply' — the model's final answer or the tool calls it
wants made — or a 'BackendError'.
__Contract__: implementations must not throw IO exceptions — every failure
comes back as 'Left'.
-}
newtype Backend m = Backend
  { runBackendChat :: [ContextItem] -> Text -> Value -> [ToolDef] -> m (Either BackendError BackendReply)
  }

{- | A single 'Backend' call's reply: either the model's final answer, or
the tool calls it wants made before it will answer. A backend with no
tool concept (a mock, a file-replay backend) always returns 'BackendAnswer'.
-}
data BackendReply
  = {- | The model's final response text (still raw JSON, same as the old
    'Backend' always returned).
    -}
    BackendAnswer Text
  | {- | The model wants these tool calls made before it will answer.
      More than one call means parallel tool calls: dispatch each, then
      send all the results back together on the next 'runBackendChat' call.
    -}
    BackendToolCalls [ToolCall]
  deriving (Eq, Show)

{- | Why a 'Backend' call failed, distinguishing a refusal from a truncated
reply and from a transport failure.

A refusal is terminal: the model declining outright is not something a
re-prompt fixes, so retrying just spends retry budget on real API calls
before reporting the same refusal anyway. A transport failure (a 429, a
500, a timeout, a decode failure) is exactly what a retry is for.

'BackendRefusal' carries what a refusal actually offers — a provider-defined
category, when the provider names one, and the provider's own message text
— rather than being a bare nullary constructor, so that something
populating it from a real provider's error payload (the shroom arc's todo
14, from @baikai@\'s @BaikaiError@) has fields to fill rather than only a
flag to set. Both fields are shaped after what @baikai@ can actually
supply: its @refusalCategory :: Maybe Text@ is only ever populated on
Anthropic's streaming path and is 'Nothing' everywhere else, so it stays
optional here too; its @message :: Text@ is always present and already
has any category or explanation folded in by the time it reaches a
caller, so there is nothing left over for a separate "reason" field to
carry.
-}
data BackendError
  = -- | The model declined to answer at all.
    BackendRefusal
      { refusalCategory :: Maybe Text
      {- ^ Provider-reported refusal category (e.g. a content-policy tag),
      when the provider names one. Pass through whatever the provider
      calls it.
      -}
      , refusalMessage :: Text
      -- ^ The provider's own message text, for logging\/debugging.
      }
  | {- | The reply was cut off by the output-token or context limit (a
    @finish_reason: "length"@ stop) and so is, at best, half a JSON
    document. Carries whatever text arrived before the cut, for logging
    only — it is not a usable answer. Unlike a refusal this is worth a
    retry, but unlike a transport failure it should not be retried blind: a
    re-prompt that says nothing gets the same overlong answer again, so the
    retry loop tells the model its reply was cut off and asks for a shorter
    one.
    -}
    BackendTruncated Text
  | -- | Anything else: an HTTP failure, a decode failure, a timeout, ...
    BackendTransportError Text
  deriving (Eq, Show)
