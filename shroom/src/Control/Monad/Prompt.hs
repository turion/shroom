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
module Control.Monad.Prompt (module Control.Monad.Prompt, module Control.Monad.Prompt.Core, module Control.Monad.Prompt.Promptable) where

-- base
import Control.Monad.IO.Class (MonadIO (..))
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

-- unliftio
import UnliftIO (MonadUnliftIO, withRunInIO)
import UnliftIO.Async (concurrently)

import Data.Aeson (FromJSON, Value (..), eitherDecodeStrictText)

-- sop-core
import Data.SOP (All, NP (..), SListI)

-- shroom (internal)
import Control.Monad.Prompt.Core (ContextItem (..), PromptT (..))
import Control.Monad.Prompt.Promptable (Promptable (..))
import Control.Monad.Prompt.Schema (ToolDef, ToolDispatcher, schemaWithDefs)
import Control.Monad.Prompt.Tool (ToolHandler, Toolable, makeDispatcher, toolDefsRaw)

-- universe-base
import Data.Universe.Class (universe)

-- shroom
import Data.Shroom.Class (describeProperties, description, propertyHolds)

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

-- | Like 'prompt', but with an extra piece of context scoped to this request only.
promptWith :: (Promptable a) => Text -> PromptT m a
promptWith txt = WithContext (UserMessage txt) prompt

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

{- | Type class for LLM backends.

'runChatWithTools' is the sole required method.  Backends receive the full
list of available tools and a dispatch callback; they may use them or ignore
them freely.

__Contract__: implementations must not throw IO exceptions — all errors must
be returned as @Left@.
-}
class LLMBackend cfg where
  {- | Call the LLM with optional tool support.

    * @history@ — accumulated conversation so far.
    * @typeDescription@ — appended as a final user turn; describes the
      expected output type.
    * @schema@ — OpenAPI JSON schema for the expected type.
    * @tools@ — available tool definitions; empty list means no tools.
    * @dispatch@ — callback to invoke a tool by name with a JSON input.
    * @maxToolSteps@ — maximum tool-call iterations before giving up.

    Returns the raw JSON text of the final (non-tool) response, or an error.
  -}
  runChatWithTools ::
    (MonadIO m) =>
    cfg ->
    -- | runner config (carries 'debugLog' for tool event logging)
    PromptConfig ->
    [ContextItem] ->
    -- | type description
    Text ->
    -- | output schema
    Value ->
    -- | available tools
    [ToolDef] ->
    ToolDispatcher m ->
    -- | max tool steps
    Maybe Int ->
    m (Either Text Text)

{- | Convenience wrapper: chat without tools.
Calls 'runChatWithTools' with an empty tool list and zero tool steps.
-}
runChat :: (LLMBackend cfg, MonadIO m) => cfg -> [ContextItem] -> Text -> Value -> m (Either Text Text)
runChat cfg ctx typeDesc schema =
  runChatWithTools cfg defaultPromptConfig ctx typeDesc schema [] (\_ _ -> pure (Left "no tools")) (Just 0)

{- | Like 'runPromptT' but without tools.
Convenient for callers that don't need tool use; equivalent to
@'runPromptT' cfg 'Nil'@.
-}
runPromptTNoTools ::
  (LLMBackend cfg, MonadUnliftIO m) =>
  PromptConfig ->
  PromptT m a ->
  PromptResultT cfg m a
runPromptTNoTools cfg = runPromptT cfg Nil

-- * Runner configuration

-- | Model-independent configuration for the prompt runner.
data PromptConfig = PromptConfig
  { maxRetries :: Int
  {- ^ Maximum number of re-prompts when a parsed value fails property
  validation.  Default: 3.
  -}
  , maxToolSteps :: Maybe Int
  {- ^ Maximum number of tool-call iterations per 'prompt' call before giving
  up.  Only relevant when tools are supplied to 'runPromptT'.  Default: 10.
  'Nothing' means no limit.
  -}
  , debugLog :: Maybe (Text -> IO ())
  {- ^ Optional logger called before each LLM call and after each response.
  Receives a human-readable 'Text' summary.  Pass @Just TIO.putStrLn@ for
  stdout, or @tasty-hunit@\'s @step@ callback in tests.  Default: 'Nothing'.
  -}
  }

-- | Default 'PromptConfig': up to 3 retries, up to 10 tool steps, no debug logging.
defaultPromptConfig :: PromptConfig
defaultPromptConfig = PromptConfig {maxRetries = 3, maxToolSteps = Just 10, debugLog = Nothing}

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

Pass an 'NP' of 'ToolHandler's to enable tool use; use 'Nil' for no tools.

Run the result with 'runPromptResultTWith'.
-}
runPromptT ::
  forall tools cfg m a.
  (LLMBackend cfg, MonadUnliftIO m, SListI tools, All Toolable tools) =>
  PromptConfig ->
  -- | Available tools; pass 'Nil' for none.
  NP ToolHandler tools ->
  PromptT m a ->
  PromptResultT cfg m a
runPromptT promptCfg handlers p = do
  cfg <- ask
  fst <$> go cfg [] p
  where
    toolDefs = toolDefsRaw handlers
    dispatch = makeDispatcher handlers
    -- Returns the result together with the context as it stands after execution.
    -- Context accumulated inside a sub-program is visible to all subsequent
    -- steps in the same 'Bind' chain.
    go :: forall b. cfg -> [ContextItem] -> PromptT m b -> PromptResultT cfg m (b, [ContextItem])
    go _cfg ctx (AddContext item) = pure ((), ctx <> [item])
    go cfg ctx (WithContext item inner) = do
      (a, _) <- go cfg (ctx <> [item]) inner
      pure (a, ctx)
    go cfg ctx (PromptSingle :: PromptT m b) = do
      let prx = Proxy @b
          typeDesc = description prx
          schema = schemaWithDefs prx
          checkProps a =
            [ desc
            | prop <- universe
            , not (propertyHolds a prop)
            , Just desc <- [describeProperties prx prop]
            ]
      (a, rawTxt) <- attempt cfg ctx typeDesc schema checkProps (maxRetries promptCfg) (maxRetries promptCfg)
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

    logDebug :: (MonadIO n) => Text -> n ()
    logDebug msg = case promptCfg.debugLog of
      Nothing -> pure ()
      Just fn -> liftIO (fn msg)

    -- Returns both the parsed value and the raw response text (for AssistantMessage).
    -- totalRetries is the original maxRetries value (fixed); retriesLeft decrements on each retry.
    attempt :: forall c. (FromJSON c) => cfg -> [ContextItem] -> Text -> Value -> (c -> [Text]) -> Int -> Int -> PromptResultT cfg m (c, Text)
    attempt cfg ctx typeDesc schema checkProps totalRetries retriesLeft = do
      let attemptNum = totalRetries - retriesLeft + 1
          separator = T.replicate 60 "─"
          renderItem (SystemMessage t) = "[system]    " <> t
          renderItem (UserMessage t) = "[user]      " <> t
          renderItem (AssistantMessage t) = "[assistant] " <> t
      logDebug $
        T.unlines $
          [ separator
          , "LLM CALL  attempt "
              <> pack (show attemptNum)
              <> "/"
              <> pack (show (totalRetries + 1))
              <> "  →  "
              <> typeDesc
          ]
            <> fmap renderItem ctx
            <> [ separator
               ]
      result <- runChatWithTools cfg promptCfg ctx typeDesc schema toolDefs (\n v -> lift (dispatch n v)) (maxToolSteps promptCfg)
      let attemptsStr = "(" <> pack (show attemptNum) <> " attempt(s))"
      case result of
        Left err -> do
          logDebug $ "✗ ERROR: " <> err
          if retriesLeft <= 0
            then throwError $ err <> "\n" <> attemptsStr
            else attempt cfg ctx typeDesc schema checkProps totalRetries (retriesLeft - 1)
        Right rawTxt ->
          case eitherDecodeStrictText' rawTxt of
            Left parseErr -> do
              logDebug $ "✗ PARSE FAIL: " <> parseErr
              if retriesLeft <= 0
                then throwError $ parseErr <> "\n" <> attemptsStr
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
                  attempt cfg retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)
            Right a ->
              let failDescs = checkProps a
               in if null failDescs
                    then do
                      logDebug $ "✓ " <> rawTxt
                      pure (a, rawTxt)
                    else do
                      logDebug $ "✗ VALIDATION FAIL: " <> T.intercalate ", " failDescs
                      if retriesLeft <= 0
                        then
                          throwError $
                            "Property validation failed:\n"
                              <> T.intercalate "\n" (fmap ("- " <>) failDescs)
                              <> "\n"
                              <> attemptsStr
                        else do
                          let retryCtx =
                                ctx
                                  <> [ AssistantMessage rawTxt
                                     , UserMessage $
                                         "IMPORTANT: Your previous response was rejected because it violated required properties.\n"
                                           <> "Violated properties:\n"
                                           <> T.unlines (fmap ("- " <>) failDescs)
                                           <> "\nGenerate a new, corrected JSON response that satisfies ALL properties listed above."
                                     ]
                          attempt cfg retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)

    eitherDecodeStrictText' :: forall c. (FromJSON c) => Text -> Either Text c
    eitherDecodeStrictText' t = case eitherDecodeStrictText t of
      Left err -> Left (mconcat ["JSON decode error: ", t, "\n", pack err])
      Right a -> Right a

{- | Run a 'PromptResultT' with the given backend configuration, returning
either an error message or the final result.
-}
runPromptResultTWith :: cfg -> PromptResultT cfg m a -> m (Either Text a)
runPromptResultTWith cfg (PromptResultT r) = runExceptT $ runReaderT r cfg
