{- |
Module      : Control.Monad.Prompt.Backend
Description : The connection that runs a prompt program's chat calls on a language model host.

A shroom program says what to ask for.
'Control.Monad.Prompt.Effect.runPrompt' interprets it, turns each request into chat
calls with retries and tool rounds, and sends every chat call through a t'Backend'
to a language model host. A t'Backend' is the connection to that host. Use it to
run a program on a real model, or to plug in a transport of your own.

A t'Backend' makes one raw chat call. It takes the conversation so far, the
description and JSON schema of the expected result type and the tools on offer,
and returns a 'BackendReply' or a 'BackendError'. The retry and tool loop around
that call is in 'Control.Monad.Prompt.Effect.runPrompt'.

A t'Backend' is a plain record, not a type class, so building one needs no
instance and no dependency on shroom's other modules. An adapter can turn a
@ToSchema@ instance into the schema the call needs with 'schemaWithDefs'.
'Control.Monad.Prompt.FileMock.fileMockBackend' replays canned responses from
files; the @shroom-baikai@ package builds a t'Backend' for real models.

As a trivial example, this backend answers every request with the same text:

@
constantBackend :: (Applicative m) => Text -> Backend m
constantBackend answer = Backend $ \\_ _ _ _ -> pure $ Right $ BackendAnswer answer
@
-}
module Control.Monad.Prompt.Backend (
  -- * Backends
  Backend (..),
  BackendReply (..),
  BackendError (..),

  -- * Conversation
  ContextItem (..),
  renderContextItems,

  -- * Tools
  ToolDef (..),
  ToolCall (..),
  ToolResult (..),

  -- * Schemas
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

{- | A tool call the model requested, either in a 'BackendToolCalls' reply or,
once dispatched, replayed in a 'ToolCallMessage'.
-}
data ToolCall = ToolCall
  { toolCallId :: Text
  {- ^ The provider's own id for this call, echoed back unchanged in the
  matching t'ToolResult' so the provider can link the two.
  -}
  , toolCallName :: Text
  , toolCallArguments :: Value
  }
  deriving (Eq, Show)

-- | The outcome of dispatching one t'ToolCall', linked to it by 'toolResultId'.
data ToolResult = ToolResult
  { toolResultId :: Text
  -- ^ Echoes the originating t'ToolCall'\'s 'toolCallId'.
  , toolResultName :: Text
  , toolResultOutcome :: Either Text Text
  {- ^ 'Left' — the tool failed, with a message for the model. 'Right' —
  the tool's own result text.
  -}
  }
  deriving (Eq, Show)

-- * Context items

{- | One item of the conversation history. A t'Backend' maps each item to the
message of its transport that has the matching role.
-}
data ContextItem
  = -- | Instructions or persona, sent before any user turns.
    SystemMessage Text
  | -- | A user turn in the conversation.
    UserMessage Text
  | {- | A previous model response. Appended after each successful request so
      that later prompts can refer to earlier answers.
    -}
    AssistantMessage Text
  | {- | The tool calls the model requested in one turn. Parallel calls travel
      together, so they replay as one assistant turn.
    -}
    ToolCallMessage [ToolCall]
  | {- | The results of dispatching the calls of a 'ToolCallMessage', one per
      call, each linked to its call by 'toolResultId'.
    -}
    ToolResultMessage [ToolResult]
  deriving (Eq, Show)

{- | Render a conversation as flat 'Text', joining the items with newlines and
prefixing each with its role, except user messages. For display, or for a backend
without structured message histories.
-}
renderContextItems :: [ContextItem] -> Text
renderContextItems = T.intercalate "\n" . fmap render
  where
    render (SystemMessage t) = "[system] " <> t
    render (UserMessage t) = t
    render (AssistantMessage t) = "[assistant] " <> t
    render (ToolCallMessage calls) = "[tool call] " <> T.intercalate ", " (fmap toolCallName calls)
    render (ToolResultMessage results) = "[tool result] " <> T.intercalate ", " (fmap toolResultName results)

{- | A chat backend, reduced to the one call every backend must make.

__Contract:__ an implementation must not throw IO exceptions; every failure
comes back as a 'BackendError'.
-}
newtype Backend m = Backend
  { runBackendChat :: [ContextItem] -> Text -> Value -> [ToolDef] -> m (Either BackendError BackendReply)
  {- ^ Makes the chat call. The arguments, in order, are:

  * the conversation so far,
  * the description of the expected result type, appended as the final turn,
  * the JSON schema the response must satisfy,
  * the tools on offer for this call; empty means none.
  -}
  }

{- | The reply of one t'Backend' call: the model's final answer, or the tool
calls it wants made before it answers. A backend without tools, such as a mock,
always returns 'BackendAnswer'.
-}
data BackendReply
  = -- | The model's final response text, still raw JSON.
    BackendAnswer Text
  | {- | The model wants these tool calls made before it will answer.
      More than one call means parallel tool calls: dispatch each, then
      send all the results back together on the next 'runBackendChat' call.
    -}
    BackendToolCalls [ToolCall]
  deriving (Eq, Show)

{- | Why a t'Backend' call failed. The three cases are handled differently when
retrying: a refusal is terminal, a truncated reply is retried with a request
for a shorter one, and a transport failure is retried unchanged.
-}
data BackendError
  = -- | The model declined to answer at all; no re-prompt fixes that.
    BackendRefusal
      { refusalCategory :: Maybe Text
      -- ^ The category the provider names for the refusal, if it names one.
      , refusalMessage :: Text
      -- ^ The provider's own message text, for logging.
      }
  | {- | The reply was cut off by a length limit and is, at best, half a JSON
    document. Carries the text that arrived, for logging only.
    -}
    BackendTruncated Text
  | -- | Anything else: an HTTP failure, a decode failure, a timeout.
    BackendTransportError Text
  deriving (Eq, Show)
