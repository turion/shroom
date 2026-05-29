{- | A monad transformer for building structured LLM conversations.

'PromptT' lets you accumulate context and request typed responses from a
language model backend.  The DSL has two primitives:

* 'context' — add a piece of text to the global conversation context
* 'prompt' / 'promptWith' — ask the model to produce a value of a specific type

Because 'PromptT' is a monad, you can chain multiple prompts together
(/prompt chaining/): each 'prompt' call is a separate LLM request, but all
of them see the global context accumulated so far.

@
do context "Alice is 30 years old."
   user  <- prompt \@User       -- first LLM call
   score <- promptWith \@Score  -- second LLM call, still sees "Alice is 30..."
              "Rate this user's awesomeness from 0 to 100."
   pure (user, score)
@

Global context (added via 'context') is preserved across all prompts in the
chain.  Prompt-local context (the argument to 'promptWith') is only sent with
that one prompt and does not accumulate.

Run a 'PromptT' by choosing a backend ('LLMBackend') and calling 'runPromptT'
inside 'runPromptResultTWith'.  Two backends are provided:

* "Control.Monad.Prompt" itself — 'AnthropicConfig' for the Claude API
* "Control.Monad.Prompt.Ollama" — 'OllamaBackendConfig' for local Ollama models

Example:

@
result <- runPromptResultTWith cfg $ runPromptT $ do
  context "The user's name is Alice."
  prompt \@User
@
-}
module Control.Monad.Prompt (module Control.Monad.Prompt) where

-- base
import Control.Applicative (Alternative (..))
import Control.Exception (SomeException, try)
import Control.Monad (MonadPlus (..))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (Foldable (..))
import Data.Proxy (Proxy (..))
import Numeric.Natural (Natural)

-- mtl
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Reader (MonadReader (..))

-- transformers
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT (..))

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- witherable
import Witherable (Filterable (..), (<&?>))

-- vector
import Data.Vector qualified as V

-- claude
import Claude.V1
import Claude.V1.Messages

-- unliftio
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async (concurrently)

import Data.Aeson (FromJSON, Value (..), eitherDecodeStrictText, toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.OpenApi (Referenced (..), ToSchema, declareSchemaRef, getReference)
import Data.OpenApi.Declare (runDeclare)

-- universe-base
import Data.Universe.Class (universe)

-- shroom
import Data.Describe
import Data.Describe qualified as D

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
  -- | Request a typed value from the model using the accumulated context.
  Prompt :: (ToSchema a, FromJSON a, Describe a) => PromptT m a
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

{- | Scope a 'ContextItem' to a sub-program.
The item is only visible within @p@ and does not persist afterwards.
-}
withContextItem :: ContextItem -> PromptT m a -> PromptT m a
withContextItem = WithContext

{- | Scope a piece of user-turn text to a sub-program.
The context is only visible within @p@ and does not persist afterwards.
-}
withContext :: Text -> PromptT m a -> PromptT m a
withContext txt = WithContext (UserMessage txt)

{- | Append a 'ContextItem' to the context for the remainder of the current
do-block (via 'Bind').
-}
addContextItem :: ContextItem -> PromptT m ()
addContextItem = AddContext

{- | Append a piece of user-turn text to the context for the remainder of the
current do-block (via 'Bind'). Context does not persist past the enclosing scope.
-}
context :: Text -> PromptT m ()
context txt = AddContext (UserMessage txt)

{- | Request a value of type @a@ from the model.
The model sees the accumulated context plus the type description.
-}
prompt :: (ToSchema a, FromJSON a, Describe a) => PromptT m a
prompt = Prompt

-- | Like 'prompt', but with an extra piece of context scoped to this request only.
promptWith :: (ToSchema a, FromJSON a, Describe a) => Text -> PromptT m a
promptWith txt = WithContext (UserMessage txt) Prompt

{- | Run two independent prompts in parallel, returning both results.
Both branches see the same context snapshot at the point of the call;
'context' calls inside a branch are local to that branch.
-}
promptPar :: PromptT m a -> PromptT m b -> PromptT m (a, b)
promptPar pa pb = (,) <$> pa <*> pb

{- | Run a list of independent prompts in parallel, returning all results.
All branches see the same context snapshot at the point of the call;
'context' calls inside a branch are local to that branch.
-}
promptsParallel :: [PromptT m a] -> PromptT m [a]
promptsParallel = sequenceA

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

-- * Backend abstraction

{- | Type class for LLM backends.  Each instance specifies a configuration
type and provides a single-turn chat operation: given the accumulated
conversation history and a type description, return the raw JSON text
produced by the model.

__Contract__: implementations of 'runChat' must not throw IO exceptions.
All errors (HTTP failures, timeouts, API errors) must be caught and
returned as @Left@.
-}
class LLMBackend cfg where
  {- | @runChat cfg history typeDescription schema@ calls the LLM and returns
    the raw response text (expected to be valid JSON for the requested type).

    @history@ is the conversation so far as a list of 'ContextItem' values.
    @typeDescription@ is appended as a final user turn by the backend.
    @schema@ is the OpenAPI JSON schema for the expected type, encoded as a
    JSON 'Value'.  Backends may use it to enforce structured output.
  -}
  runChat :: (MonadIO m) => cfg -> [ContextItem] -> Text -> Value -> m (Either Text Text)

-- * Anthropic backend

-- | Configuration for the Anthropic Claude API.
data AnthropicConfig = AnthropicConfig
  { apiKey :: Text
  -- ^ Your Anthropic API key.
  , model :: Text
  -- ^ Model identifier, e.g. @\"claude-haiku-4-5-20251001\"@.
  , maxTokens :: Natural
  -- ^ Maximum number of tokens in the model response. Default: 4096.
  }

{- | Make a configuration for the Anthropic backend.

Provide a specified API key,
reasonable defaults for other parameters: Claude Haiku 4-5-20251001 and max tokens 4096.
-}
mkAnthropicConfig ::
  -- | Your Anthropic API key.
  Text ->
  AnthropicConfig
mkAnthropicConfig apiKey =
  AnthropicConfig
    { apiKey = apiKey
    , model = "claude-haiku-4-5-20251001"
    , maxTokens = 4096
    }

{- | Build a JSON schema 'Value' for type @a@ that includes all referenced
sub-schemas in a @$defs@ section, using openapi3's 'declareSchemaRef'.
This avoids the @#\/components\/schemas\/X@ references that Anthropic rejects.
-}
schemaWithDefs :: forall a. (ToSchema a) => Proxy a -> Value
schemaWithDefs prx =
  let (defs, ref) = runDeclare (declareSchemaRef prx) mempty
      rootSchema = case ref of
        Inline s -> toJSON s
        Ref r -> Object (KM.singleton "$ref" (String ("#/$defs/" <> getReference r)))
      defsJson = toJSON defs
   in case defsJson of
        Object defsKm | not (KM.null defsKm) ->
          case rootSchema of
            Object rootKm -> fixSchemaForAnthropic (Object (KM.insert "$defs" (Object defsKm) rootKm))
            _ -> fixSchemaForAnthropic rootSchema
        _ -> fixSchemaForAnthropic rootSchema

{- | Fix a JSON schema 'Value' to satisfy Anthropic API constraints:

* Adds @"additionalProperties": false@ to every @"type": "object"@ node.
* Removes @"minimum"@ and @"maximum"@ from every @"type": "integer"@ node.
* Rewrites @$ref@ values from @#\/components\/schemas\/X@ to @#\/$defs\/X@.
-}
fixSchemaForAnthropic :: Value -> Value
fixSchemaForAnthropic (Object km) =
  let km1 = case KM.lookup "$ref" km of
        Just (String ref) -> KM.insert "$ref" (String (T.replace "#/components/schemas/" "#/$defs/" ref)) km
        _ -> km
      km' = KM.map fixSchemaForAnthropic km1
   in Object $ case KM.lookup "type" km' of
        Just (String "object") -> KM.insert "additionalProperties" (Bool False) km'
        Just (String "integer") -> KM.delete "minimum" (KM.delete "maximum" km')
        Just (String "string") -> KM.delete "format" km'
        _ -> km'
fixSchemaForAnthropic (Array vs) = Array (fmap fixSchemaForAnthropic vs)
fixSchemaForAnthropic v = v

{- | Convert a list of 'ContextItem' values to Anthropic API messages.
'SystemMessage' items are collected into the @system@ field;
'UserMessage' and 'AssistantMessage' items become @messages@.
A final user message containing @typeDesc@ is always appended.
-}
contextItemsToAnthropic :: [ContextItem] -> Text -> (Maybe SystemPrompt, [Message])
contextItemsToAnthropic items typeDesc =
  let systemTexts = [t | SystemMessage t <- items]
      mSystem = case systemTexts of
        [] -> Nothing
        ts -> Just (systemText (T.intercalate "\n" ts))
      chatItems = [item | item <- items, not (isSystem item)]
      toMsg (UserMessage t) = Just Message {role = User, content = [Content_Text {text = t, cache_control = Nothing}], cache_control = Nothing}
      toMsg (AssistantMessage t) = Just Message {role = Assistant, content = [Content_Text {text = t, cache_control = Nothing}], cache_control = Nothing}
      toMsg (SystemMessage _) = Nothing
      chatMsgs = mapMaybe toMsg chatItems
      -- Append the type description as the final user turn
      finalMsg = Message {role = User, content = [Content_Text {text = typeDesc, cache_control = Nothing}], cache_control = Nothing}
   in (mSystem, chatMsgs <> [finalMsg])
  where
    isSystem (SystemMessage _) = True
    isSystem _ = False

instance LLMBackend AnthropicConfig where
  runChat cfg ctx typeDesc schema = liftIO $ do
    result <- try @SomeException $ do
      clientEnv <- getClientEnv "https://api.anthropic.com"
      let methods = makeMethods clientEnv cfg.apiKey (Just "2023-06-01")
          (mSystem, msgs) = contextItemsToAnthropic ctx typeDesc
      resp <-
        methods.createMessage
          _CreateMessage
            { model = cfg.model
            , messages = V.fromList msgs
            , system = mSystem
            , max_tokens = cfg.maxTokens
            , output_config = Just (jsonSchemaConfig (fixSchemaForAnthropic schema))
            }
      let MessageResponse {content} = resp
          texts =
            toList $
              content <&?> \case
                ContentBlock_Text {text = t} -> Just t
                _ -> Nothing
      pure $ mconcat texts
    pure $ case result of
      Left ex -> Left ("HTTP error: " <> pack (show ex))
      Right txt -> Right txt

-- * Runner configuration

-- | Model-independent configuration for the prompt runner.
newtype PromptConfig = PromptConfig
  { maxRetries :: Int
  {- ^ Maximum number of re-prompts when a parsed value fails property
    validation.  Default: 3.
  -}
  }

-- | Default 'PromptConfig': up to 3 retries on validation failure.
defaultPromptConfig :: PromptConfig
defaultPromptConfig = PromptConfig {maxRetries = 3}

-- * PromptResultT runner

{- | The result monad for running a 'PromptT'.  Carries the backend
configuration via 'MonadReader' and surfaces errors via 'MonadError' 'Text'.
-}
newtype PromptResultT cfg m a = PromptResultT
  { getPromptResultT :: ReaderT cfg (ExceptT Text m) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError Text)

instance MonadTrans (PromptResultT cfg) where
  lift = PromptResultT . lift . lift

instance (Monad m) => MonadReader cfg (PromptResultT cfg m) where
  ask = PromptResultT ask
  local f (PromptResultT r) = PromptResultT (local f r)

{- | Interpret a 'PromptT' program against any 'LLMBackend', accumulating
context and calling the model for each 'prompt' \/ 'promptWith'.
On property-validation failure the prompt is retried up to
'maxRetries' times, each time including the previous (invalid) response
and the failing property descriptions so the model can correct itself.

'Ap' branches are executed in parallel.

Run the result with 'runPromptResultTWith'.
-}
runPromptT :: (LLMBackend cfg, MonadUnliftIO m) => PromptConfig -> PromptT m a -> PromptResultT cfg m a
runPromptT promptCfg p = do
  cfg <- ask
  fst <$> go cfg [] p
  where
    -- Returns the result together with the context as it stands after execution.
    -- Context accumulated inside a sub-program is visible to all subsequent
    -- steps in the same 'Bind' chain.
    go ::
      (LLMBackend cfg, MonadUnliftIO m) =>
      cfg -> [ContextItem] -> PromptT m a -> PromptResultT cfg m (a, [ContextItem])
    go _cfg ctx (AddContext item) = pure ((), ctx <> [item])
    go cfg ctx (WithContext item inner) = do
      (a, _) <- go cfg (ctx <> [item]) inner
      pure (a, ctx)
    go cfg ctx (Prompt :: PromptT m a) = do
      let prx = Proxy @a
          typeDesc = D.description prx
          schema = schemaWithDefs prx
          checkProps a =
            [ desc
            | prop <- universe
            , not (propertyHolds a prop)
            , Just desc <- [describeProperties prx prop]
            ]
      (a, rawTxt) <- attempt cfg ctx typeDesc schema checkProps (maxRetries promptCfg)
      pure (a, ctx <> [AssistantMessage rawTxt])
    go _cfg ctx (Pure a) = pure (a, ctx)
    go _cfg ctx (Lift m) = (,ctx) <$> lift m
    go cfg ctx (Bind q k) = do
      (a, ctx') <- go cfg ctx q
      go cfg ctx' (k a)
    go cfg ctx (Ap pf pa) = do
      let runF = fmap fst <$> runPromptResultTWith cfg (go cfg ctx pf)
          runA = fmap fst <$> runPromptResultTWith cfg (go cfg ctx pa)
      (rf, ra) <- lift $ withRunInIO $ \runInIO ->
        concurrently (runInIO runF) (runInIO runA)
      case (rf, ra) of
        (Left e, _) -> throwError e
        (_, Left e) -> throwError e
        (Right f, Right a) -> pure (f a, ctx)
    go _cfg _ctx (Fail msg) = throwError msg
    go cfg ctx (Alt left right) =
      catchError (go cfg ctx left) (\_ -> go cfg ctx right)

    -- Returns both the parsed value and the raw response text (for AssistantMessage).
    attempt cfg ctx typeDesc schema checkProps retriesLeft = do
      result <- runChat cfg ctx typeDesc schema
      case result of
        Left err -> throwError err
        Right rawTxt ->
          case eitherDecodeStrictText' rawTxt of
            Left parseErr ->
              if retriesLeft <= 0
                then throwError parseErr
                else do
                  let retryCtx =
                        ctx
                          <> [ AssistantMessage rawTxt
                             , UserMessage $
                                 "IMPORTANT: Your previous response could not be parsed.\n"
                                   <> "Parse error:\n"
                                   <> parseErr
                                   <> "\n\nGenerate a new, corrected JSON response that matches the required schema exactly."
                             ]
                  attempt cfg retryCtx typeDesc schema checkProps (retriesLeft - 1)
            Right a ->
              let failDescs = checkProps a
               in if null failDescs
                    then pure (a, rawTxt)
                    else
                      if retriesLeft <= 0
                        then
                          throwError $
                            "Property validation failed after retries:\n"
                              <> T.intercalate "\n" (fmap ("- " <>) failDescs)
                        else do
                          let retryCtx =
                                ctx
                                  <> [ AssistantMessage rawTxt
                                     , UserMessage $
                                         "IMPORTANT: Your previous response was rejected because it violated required invariants.\n"
                                           <> "Violated invariants:\n"
                                           <> T.unlines (fmap ("- " <>) failDescs)
                                           <> "\nGenerate a new, corrected JSON response that satisfies ALL invariants listed above."
                                     ]
                          attempt cfg retryCtx typeDesc schema checkProps (retriesLeft - 1)

    eitherDecodeStrictText' :: (FromJSON a) => Text -> Either Text a
    eitherDecodeStrictText' t = case eitherDecodeStrictText t of
      Left err -> Left (mconcat ["JSON decode error: ", t, "\n", pack err])
      Right a -> Right a

{- | Run a 'PromptResultT' with the given backend configuration, returning
either an error message or the final result.
-}
runPromptResultTWith :: cfg -> PromptResultT cfg m a -> m (Either Text a)
runPromptResultTWith cfg (PromptResultT r) = runExceptT $ runReaderT r cfg
