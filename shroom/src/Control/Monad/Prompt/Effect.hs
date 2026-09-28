{- | shroom's program vocabulary expressed as 'effectful' effects, living
alongside "Control.Monad.Prompt" ('Control.Monad.Prompt.Core.PromptT') rather
than in place of it — the tree still builds with both. Todo 12 of the
shroom arc deletes 'Control.Monad.Prompt.Core.PromptT'; this module is the
landing spot that deletion repoints onto.

== Mapping from 'Control.Monad.Prompt.Core.PromptT'

* 'Control.Monad.Prompt.Core.AddContext' — context held in
  "Effectful.State.Static.Local"
* 'Control.Monad.Prompt.Core.WithContext' — @local@-style scoping over that
  state ('withContextItem' \/ 'withContext')
* 'Control.Monad.Prompt.Core.PromptSingle' — the 'Prompt' dynamic effect
* 'Control.Monad.Prompt.Core.Pure', 'Control.Monad.Prompt.Core.Bind',
  'Control.Monad.Prompt.Core.Lift' — 'Eff' is already a monad
* 'Control.Monad.Prompt.Core.Fail' — "Effectful.Error.Static" with 'Text'
  ('failWith')
* 'Control.Monad.Prompt.Core.Alt' — 'catchError' plus context restoration
  ('orElse')
* 'Control.Monad.Prompt.Core.Ap' — "Effectful.Concurrent.Async"
  ('promptPar' \/ 'promptsParallel')

'runPrompt' interprets 'Prompt' over the existing 'LLMBackend' class, so
"Control.Monad.Prompt.Anthropic" and "Control.Monad.Prompt.Ollama" need no
changes at this revision. 'LLMBackend' reports a backend error as a bare
'Text', which cannot yet distinguish a refusal (terminal — no re-prompt
fixes a model declining outright) from a transport failure (a 429, a 500 or
a timeout — exactly what a retry is for). See 'classifyBackendError' for
where that distinction will land once todo 11 introduces a shroom-owned
error type that can tell the two apart; todo 12 repoints this interpreter
onto it.
-}
module Control.Monad.Prompt.Effect (
  -- * The 'Prompt' effect
  Prompt,
  prompt,
  promptWith,
  promptPar,
  promptsParallel,
  runPrompt,
  runPromptResultEff,

  -- * Context ("Effectful.State.Static.Local")
  addContextItem,
  context,
  withContextItem,
  withContext,

  -- * Failure and fallback ("Effectful.Error.Static")
  failWith,
  orElse,
) where

-- base
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, Value, eitherDecodeStrictText)

-- openapi3
import Data.OpenApi (ToSchema)

-- universe-base
import Data.Universe.Class (universe)

-- effectful
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, runEff, (:>))
import Effectful.Concurrent.Async (Concurrent, concurrently, mapConcurrently, runConcurrent)
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, catchError, runErrorNoCallStack, throwError)
import Effectful.State.Static.Local (State, evalState, get, modify, put)

-- shroom
import Control.Monad.Prompt (ContextItem (..), LLMBackend (..), PromptConfig (..))
import Control.Monad.Prompt.Schema (schemaWithDefs)
import Data.Shroom.Class (Surveyable, describeProperties, description, propertyHolds)

-- * The Prompt effect

{- | Dynamic effect mirroring 'Control.Monad.Prompt.Core.PromptSingle': request
a typed value from the model, using whatever context is currently held in
"Effectful.State.Static.Local".
-}
data Prompt :: Effect where
  RequestPrompt :: (Surveyable a, ToSchema a, FromJSON a) => Prompt m a

type instance DispatchOf Prompt = Dynamic

-- | Request a value of type @a@ from the LLM. Mirrors 'Control.Monad.Prompt.Promptable.prompt'.
prompt :: forall a es. (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) => Eff es a
prompt = send RequestPrompt

-- | Like 'prompt', with an extra piece of context scoped to this request only.
promptWith ::
  (Prompt :> es, State [ContextItem] :> es, Surveyable a, ToSchema a, FromJSON a) =>
  Text ->
  Eff es a
promptWith txt = withContext txt prompt

{- | Run two programs concurrently via "Effectful.Concurrent.Async", returning
both results. Unlike 'Control.Monad.Prompt.Core.PromptT'\'s @('<*>')@, which
forked silently, this is an explicit call.
-}
promptPar :: (Concurrent :> es) => Eff es a -> Eff es b -> Eff es (a, b)
promptPar = concurrently

-- | Run a list of programs concurrently, returning all results.
promptsParallel :: (Concurrent :> es) => [Eff es a] -> Eff es [a]
promptsParallel = mapConcurrently id

-- * Context

-- | Append a 'ContextItem' for the remainder of the current computation.
addContextItem :: (State [ContextItem] :> es) => ContextItem -> Eff es ()
addContextItem item = modify (\ctx -> ctx <> [item] :: [ContextItem])

-- | Append a piece of user-turn text for the remainder of the current computation.
context :: (State [ContextItem] :> es) => Text -> Eff es ()
context = addContextItem . UserMessage

{- | Scope a 'ContextItem' to a sub-computation: visible only within @act@, and
not carried past it — including anything @act@ itself added, mirroring
'Control.Monad.Prompt.Core.WithContext'.
-}
withContextItem :: (State [ContextItem] :> es) => ContextItem -> Eff es a -> Eff es a
withContextItem item act = do
  (saved :: [ContextItem]) <- get
  put (saved <> [item] :: [ContextItem])
  a <- act
  put saved
  pure a

-- | Scope a piece of user-turn text to a sub-computation.
withContext :: (State [ContextItem] :> es) => Text -> Eff es a -> Eff es a
withContext = withContextItem . UserMessage

-- * Failure and fallback

-- | Always fail with the given message.
failWith :: (Error Text :> es) => Text -> Eff es a
failWith = throwError

{- | Try the left computation; on failure, restore the context to how it stood
before the attempt, discarding anything the failing branch added, and run
the right computation from there. Mirrors 'Control.Monad.Prompt.Core.Alt'.
-}
orElse :: (Error Text :> es, State [ContextItem] :> es) => Eff es a -> Eff es a -> Eff es a
orElse l r = do
  (saved :: [ContextItem]) <- get
  l `catchError` \_ (_ :: Text) -> put saved >> r

-- * Backend error classification

{- | Whether a backend error should consume retry budget.

A refusal is terminal — the model declining outright is not something a
re-prompt fixes, so retrying just spends 'maxRetries' real API calls before
reporting the same refusal anyway. A transport failure (a 429, a 500, a
timeout) is exactly what a retry is for.

'LLMBackend' surfaces both as an indistinguishable bare 'Text', so this
always answers 'BackendTransportError' at this revision: the
'BackendRefusal' branch in 'runPrompt' is unreachable until todo 11
introduces a shroom-owned error type that separates the two, and todo 12
repoints this interpreter onto it. Do not fake the distinction by matching
on the error text in the meantime.
-}
data BackendErrorKind = BackendRefusal | BackendTransportError

classifyBackendError :: Text -> BackendErrorKind
classifyBackendError _ = BackendTransportError

-- * Interpreter

{- | Interpret 'Prompt' against any 'LLMBackend', threading context through
"Effectful.State.Static.Local" and surfacing failure via
"Effectful.Error.Static". On a parse failure or a property violation the
request is retried up to 'maxRetries' times, each time appending the
offending response (as an 'AssistantMessage') and a 'UserMessage' naming
what went wrong — the same behaviour as
'Control.Monad.Prompt.runPromptT'\'s retry loop. 'debugLog' emits the same
before\/after-call lines with the same OK \/ PARSE FAIL \/ VALIDATION FAIL \/
ERROR outcomes.

Interprets over 'LLMBackend' directly, without tool support — tool use
is not part of this todo's scope; todo 12 is where this interpreter is
repointed onto todo 11's narrower interface.
-}
runPrompt ::
  forall cfg es a.
  (LLMBackend cfg, IOE :> es, Error Text :> es, State [ContextItem] :> es) =>
  cfg ->
  PromptConfig ->
  Eff (Prompt : es) a ->
  Eff es a
runPrompt cfg promptCfg = interpret $ \_ (RequestPrompt :: Prompt (Eff localEs) b) -> do
  ctx <- get
  let prx = Proxy @b
      typeDesc = description prx
      schema = schemaWithDefs prx
      checkProps v =
        [ desc
        | p <- universe
        , not (propertyHolds v p)
        , Just desc <- [describeProperties prx p]
        ]
  (a, rawTxt) <- attemptLoop ctx typeDesc schema checkProps (maxRetries promptCfg) (maxRetries promptCfg)
  put (ctx <> [AssistantMessage rawTxt])
  pure a
  where
    logDebug :: Text -> Eff es ()
    logDebug msg = case promptCfg.debugLog of
      Nothing -> pure ()
      Just fn -> liftIO (fn msg)

    -- Returns both the parsed value and the raw response text (for
    -- AssistantMessage). totalRetries is the original maxRetries value
    -- (fixed); retriesLeft decrements on each retry.
    attemptLoop ::
      forall c.
      (FromJSON c) =>
      [ContextItem] ->
      Text ->
      Value ->
      (c -> [Text]) ->
      Int ->
      Int ->
      Eff es (c, Text)
    attemptLoop ctx typeDesc schema checkProps totalRetries retriesLeft = do
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
            <> [separator]
      result <- runChatWithTools cfg promptCfg ctx typeDesc schema [] (\_ _ -> pure (Left "no tools")) (Just 0)
      let attemptsStr = "(" <> pack (show attemptNum) <> " attempt(s))"
      case result of
        Left err -> do
          logDebug $ "✗ ERROR: " <> err
          case classifyBackendError err of
            BackendRefusal ->
              -- Terminal: a refusal is not something a re-prompt can fix.
              -- Unreachable at this revision — see 'classifyBackendError'.
              throwError $ err <> "\n" <> attemptsStr
            BackendTransportError ->
              if retriesLeft <= 0
                then throwError $ err <> "\n" <> attemptsStr
                else attemptLoop ctx typeDesc schema checkProps totalRetries (retriesLeft - 1)
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
                  attemptLoop retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)
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
                          attemptLoop retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)

    eitherDecodeStrictText' :: forall c. (FromJSON c) => Text -> Either Text c
    eitherDecodeStrictText' t = case eitherDecodeStrictText t of
      Left err -> Left (mconcat ["JSON decode error: ", t, "\n", pack err])
      Right v -> Right v

{- | Run a 'Prompt' program end to end against a backend: interpret 'Prompt',
thread context via "Effectful.State.Static.Local" starting from an empty
history, catch failure into an 'Either', and provide
"Effectful.Concurrent.Async" so 'promptPar' \/ 'promptsParallel' branches
run concurrently. Mirrors 'Control.Monad.Prompt.runPromptTNoTools' composed
with 'Control.Monad.Prompt.runPromptResultTWith'.
-}
runPromptResultEff ::
  (LLMBackend cfg) =>
  cfg ->
  PromptConfig ->
  Eff '[Prompt, State [ContextItem], Error Text, Concurrent, IOE] a ->
  IO (Either Text a)
runPromptResultEff cfg promptCfg program =
  runEff
    . runConcurrent
    . runErrorNoCallStack
    . evalState ([] :: [ContextItem])
    $ runPrompt cfg promptCfg program
