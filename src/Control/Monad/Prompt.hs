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
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (Foldable (..))
import Data.Proxy (Proxy (..))

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
import Witherable ((<&?>))

-- claude
import Claude.V1
import Claude.V1.Messages

-- operational
import Control.Monad.Operational
import Data.Aeson (FromJSON, Value, eitherDecodeStrictText, toJSON)
import Data.OpenApi (ToSchema, toSchema)

-- universe-base
import Data.Universe.Class (universe)

-- shroom
import Data.Describe
import Data.Describe qualified as D

-- * Prompt DSL

-- | The instruction set for the prompt DSL.
data Prompt a where
  -- | Append a piece of text to the global conversation context.
  Context :: Text -> Prompt ()
  -- | Request a typed value from the model using only the accumulated global context.
  Prompt :: (ToSchema a, FromJSON a, Describe a) => Prompt a
  {- | Like 'Prompt', but also sends an extra piece of prompt-local context that
    is /not/ carried forward into subsequent prompts.
  -}
  PromptWith :: (ToSchema a, FromJSON a, Describe a) => Text -> Prompt a

-- | A monad transformer for building LLM prompt programs.
newtype PromptT m a = PromptT {getPromptT :: ProgramT Prompt m a}
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

{- | Append a piece of text to the accumulated global context.
All subsequent 'prompt' / 'promptWith' calls will see this text.
-}
context :: Text -> PromptT m ()
context txt = PromptT $ singleton $ Context txt

{- | Request a value of type @a@ from the model.
The model sees the accumulated global context plus the type description.
-}
prompt :: (ToSchema a, FromJSON a, Describe a) => PromptT m a
prompt = PromptT $ singleton Prompt

{- | Like 'prompt', but with an extra piece of context that is only sent for
this one request and does not persist into the global context.
-}
promptWith :: (ToSchema a, FromJSON a, Describe a) => Text -> PromptT m a
promptWith txt = PromptT $ singleton $ PromptWith txt

-- * Backend abstraction

{- | Type class for LLM backends.  Each instance specifies a configuration
type and provides a single-turn chat operation: given the accumulated
context text and a type description, return the raw JSON text produced by
the model.

__Contract__: implementations of 'runChat' must not throw IO exceptions.
All errors (HTTP failures, timeouts, API errors) must be caught and
returned as @Left@.
-}
class LLMBackend cfg where
  {- | @runChat cfg context typeDescription schema@ calls the LLM and returns
    the raw response text (expected to be valid JSON for the requested type).
    @schema@ is the OpenAPI JSON schema for the expected type, encoded as a
    JSON 'Value'.  Backends may use it to enforce structured output.
  -}
  runChat :: (MonadIO m) => cfg -> Text -> Text -> Value -> m (Either Text Text)

-- * Anthropic backend

-- | Configuration for the Anthropic Claude API.
data AnthropicConfig = AnthropicConfig
  { apiKey :: Text
  -- ^ Your Anthropic API key.
  , model :: Text
  -- ^ Model identifier, e.g. @\"claude-3-5-haiku-20241022\"@.
  }

instance LLMBackend AnthropicConfig where
  runChat cfg ctx typeDesc schema = liftIO $ do
    result <- try @SomeException $ do
      clientEnv <- getClientEnv "https://api.anthropic.com"
      let methods = makeMethods clientEnv cfg.apiKey (Just "2023-06-01")
      resp <-
        methods.createMessage
          _CreateMessage
            { model = cfg.model
            , messages =
                [ Message
                    { role = User
                    , content = [Content_Text {text = ctx <> typeDesc, cache_control = Nothing}]
                    , cache_control = Nothing
                    }
                ]
            , max_tokens = 1024
            , output_config = Just (jsonSchemaConfig schema)
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

Run the result with 'runPromptResultTWith'.
-}
runPromptT :: (LLMBackend cfg, MonadIO m) => PromptConfig -> PromptT m a -> PromptResultT cfg m a
runPromptT promptCfg p = do
  cfg <- ask
  loop cfg (getPromptT p) ""
  where
    mkProxy :: Prompt a -> Proxy a
    mkProxy _ = Proxy

    loop cfg prog globalCtx = do
      command <- lift $ viewT prog
      case command of
        Return a -> pure a
        Context txt :>>= k ->
          loop cfg (k ()) (globalCtx <> "\n" <> txt)
        currentPrompt@Prompt :>>= k -> do
          let prx = mkProxy currentPrompt
              typeDesc = D.description prx
              schema = toJSON (toSchema prx)
              checkProps a =
                [ desc
                | prop <- universe
                , not (propertyHolds a prop)
                , Just desc <- [describeProperties prx prop]
                ]
          a <- attempt cfg globalCtx typeDesc schema checkProps (maxRetries promptCfg)
          loop cfg (k a) globalCtx
        currentPrompt@(PromptWith localCtx) :>>= k -> do
          let prx = mkProxy currentPrompt
              typeDesc = D.description prx
              schema = toJSON (toSchema prx)
              checkProps a =
                [ desc
                | prop <- universe
                , not (propertyHolds a prop)
                , Just desc <- [describeProperties prx prop]
                ]
          a <- attempt cfg (globalCtx <> "\n" <> localCtx) typeDesc schema checkProps (maxRetries promptCfg)
          loop cfg (k a) globalCtx

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
                          <> "\n\nIMPORTANT: Your previous response could not be parsed.\n"
                          <> "Previous (invalid) response:\n"
                          <> rawTxt
                          <> "\n\nParse error:\n"
                          <> parseErr
                          <> "\n\nGenerate a new, corrected JSON response that matches the required schema exactly."
                  attempt cfg retryCtx typeDesc schema checkProps (retriesLeft - 1)
            Right a ->
              let failDescs = checkProps a
               in if null failDescs
                    then pure a
                    else
                      if retriesLeft <= 0
                        then
                          throwError $
                            "Property validation failed after retries:\n"
                              <> T.intercalate "\n" (fmap ("- " <>) failDescs)
                        else do
                          let retryCtx =
                                ctx
                                  <> "\n\nIMPORTANT: Your previous response was rejected because it violated required invariants.\n"
                                  <> "Previous (invalid) response:\n"
                                  <> rawTxt
                                  <> "\n\nViolated invariants:\n"
                                  <> T.unlines (fmap ("- " <>) failDescs)
                                  <> "\nGenerate a new, corrected JSON response that satisfies ALL invariants listed above."
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
