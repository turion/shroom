{- | Internal module: 'PromptT' GADT and 'ContextItem'.  Do not import
directly; use "Control.Monad.Prompt" instead.
-}
module Control.Monad.Prompt.Core where

-- base
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..))
import Control.Monad.IO.Class (MonadIO (..))

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, Value)

-- openapi3
import Data.OpenApi (ToSchema)

-- shroom
import Data.Shroom.Class (Surveyable)

-- * Tool calls and results

{- | One tool call a model requested, carried either inside a 'Backend'\'s
reply (see 'Control.Monad.Prompt.Backend.BackendReply') or, once dispatched,
replayed into the conversation as part of a 'ToolCallMessage'.
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
      'prompt' call so subsequent prompts can reference prior outputs naturally.
    -}
    AssistantMessage Text
  | {- | Tool calls a model requested in a single turn (parallel calls travel
      together as one item, not one apiece, so they replay as one assistant
      turn). Appended after a 'Control.Monad.Prompt.Backend.Backend' reply
      carries 'Control.Monad.Prompt.Backend.BackendToolCalls', alongside the
      matching 'ToolResultMessage' once the calls have been dispatched.
    -}
    ToolCallMessage [ToolCall]
  | {- | The results of dispatching the calls in a 'ToolCallMessage', one
      per call, each still linked back to its call by 'toolResultId'.
    -}
    ToolResultMessage [ToolResult]
  deriving (Eq, Show)

-- * Prompt DSL

-- | A monad transformer for building LLM prompt programs.
data PromptT m a where
  {- | Append a 'ContextItem' to the accumulated context for all subsequent
    steps in the current chain. This is a persistent, non-scoped addition.
  -}
  AddContext :: ContextItem -> PromptT m ()
  {- | Add a 'ContextItem' to the LLM context for the scoped sub-program only.
    Context does not leak outside the 'WithContext' node.
  -}
  WithContext :: ContextItem -> PromptT m a -> PromptT m a
  {- | Request a typed value from the model using the accumulated context.
    The interpreter calls 'Data.Shroom.Class.description' to build the prompt.
  -}
  PromptSingle :: (Surveyable a, ToSchema a, FromJSON a) => PromptT m a
  -- | Embed a pure value into 'PromptT' without any LLM call or effect.
  Pure :: a -> PromptT m a
  -- | Lift an @m@ action into 'PromptT'. Supports 'MonadTrans' and 'MonadIO'.
  Lift :: m a -> PromptT m a
  -- | Sequentially bind: run the first action, pass the result to the continuation.
  Bind :: PromptT m a -> (a -> PromptT m b) -> PromptT m b
  {- | Apply a function to a value, running both branches in parallel.
    Both branches see the same context snapshot at the point of 'Ap'.
  -}
  Ap :: PromptT m (a -> b) -> PromptT m a -> PromptT m b
  -- | Always fail with the given error message. Used as 'empty' and via 'MonadFail'.
  Fail :: Text -> PromptT m a
  {- | Try the first branch; if it fails, run the second from the original context.
    Context accumulated inside a failing branch is discarded.
  -}
  Alt :: PromptT m a -> PromptT m a -> PromptT m a

instance Functor (PromptT m) where
  fmap f p = Bind p (Pure . f)

instance Applicative (PromptT m) where
  pure = Pure
  (<*>) = Ap

instance Monad (PromptT m) where
  (>>=) = Bind

instance MonadTrans PromptT where
  lift = Lift

instance (MonadIO m) => MonadIO (PromptT m) where
  liftIO = Lift . liftIO

instance Alternative (PromptT m) where
  empty = Fail "empty"
  (<|>) = Alt

instance MonadPlus (PromptT m)

instance MonadFail (PromptT m) where
  fail = Fail . T.pack
