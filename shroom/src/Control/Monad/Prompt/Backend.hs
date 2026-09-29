{- | The adapter seam: what an LLM transport must provide for shroom to run a
'Control.Monad.Prompt.Effect.Prompt' program against it.

'Control.Monad.Prompt.LLMBackend'\'s sole method, @runChatWithTools@, bundles
the chat call itself together with the tool list, the dispatch callback and
the step budget — eight arguments in one signature that every backend has to
satisfy whether or not it cares about tools at all, and the dispatch callback
means the /backend/ owns the tool-call loop rather than the caller. A
'Backend' only ever has to make one raw chat call: hand over the conversation
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
('Control.Monad.Prompt.Core.ContextItem', 'Control.Monad.Prompt.Core.ToolCall',
'ToolDef', 'Text', 'Value', 'BackendReply', 'BackendError') or from a package
any adapter depends on anyway ('aeson', 'text'), and nothing here forces a
second dependency on any particular transport package. Native provider
tool-calling survives through this seam too: a call carries the provider's
own call id in 'ToolCall'\'s @toolCallId@, so an adapter that gets that id
back from its provider (Anthropic's @tool_use_id@) can still link a result
to the call that produced it once the exchange has round-tripped through
'Control.Monad.Prompt.Core.ContextItem'\'s 'Control.Monad.Prompt.Core.ToolCallMessage'
\/ 'Control.Monad.Prompt.Core.ToolResultMessage', and a reply may carry more
than one call so parallel tool calls stay expressible.
'Control.Monad.Prompt.Anthropic', 'Control.Monad.Prompt.Ollama',
'Control.Monad.Prompt.FileMock' and the three mock backends in @test-utils@'s
@TestUtils@ each build a 'Backend' value alongside their existing
'Control.Monad.Prompt.LLMBackend' instance — both live side by side until
the shroom arc's todo 12 deletes the old one. A package that depends only on
@shroom@ can build one too; the shroom arc's todo 14 does exactly that,
interpreting @baikai-effectful@\'s @Baikai@ effect into a 'Backend' — that is
the worked example for anyone weighing whether writing a replacement adapter
is a weekend or a rescue.
-}
module Control.Monad.Prompt.Backend (
  Backend (..),
  BackendReply (..),
  BackendError (..),
  ToolDef (..),
) where

-- text
import Data.Text (Text)

-- aeson
import Data.Aeson (Value)

-- shroom
import Control.Monad.Prompt.Core (ContextItem, ToolCall)
import Control.Monad.Prompt.Schema (ToolDef (..))

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

{- | Why a 'Backend' call failed, distinguishing a refusal from a transport
failure.

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
  | -- | Anything else: an HTTP failure, a decode failure, a timeout, ...
    BackendTransportError Text
  deriving (Eq, Show)
