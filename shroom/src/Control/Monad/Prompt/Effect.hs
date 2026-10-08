{- | shroom's program vocabulary expressed as 'effectful' effects: context,
scoping, failure, fallback, parallelism and typed prompting, each an
ordinary effect rather than a constructor in a bespoke DSL.

== Context, scoping, failure, fallback, parallelism

* Context — held in "Effectful.State.Static.Local" ('addContextItem' \/
  'context')
* Scoping — @local@-style, over that same state ('withContextItem' \/
  'withContext')
* The typed request itself — the 'Prompt' dynamic effect ('prompt' \/
  'promptWith' \/ 'promptTools')
* Failure — "Effectful.Error.Static" with 'Text' ('failWith')
* Fallback — 'catchError' plus context restoration ('orElse')
* Parallelism — "Effectful.Concurrent.Async", explicit rather than hiding in
  an @Applicative@ instance ('promptPar' \/ 'promptsParallel')

Tool use (todo 10 of the shroom arc) is 'promptTools' plus
"Control.Monad.Prompt.Tool"\'s 'Control.Monad.Prompt.Tool.Tool' effect and
'Control.Monad.Prompt.Tool.toolBinding': a program names the tools it may
call in its own @es@, and 'promptTools' assembles the ones it is given into
a @['Control.Monad.Prompt.Schema.ToolDef']@ plus a dispatcher, which
'runPrompt'\'s own tool loop (below) drives one 'Control.Monad.Prompt.Backend.runBackendChat'
call at a time.

'runPrompt' interprets 'Prompt' against a 'Backend' — todo 11's narrow
adapter seam — which can tell a refusal ('BackendRefusal', terminal — no
re-prompt fixes a model declining outright) apart from a transport failure
('BackendTransportError', exactly what a retry is for). Because a 'Backend'
makes one raw call and nothing more, the propose\/dispatch\/observe tool
cycle runs here, one call at a time, rather than inside each backend; see
'toolLoop' (below): a round costs one step only when at least one tool call
succeeds, failed calls are free, and once the budget is spent the model gets
one final tool-free call to answer with what it already has.
-}
module Control.Monad.Prompt.Effect (
  -- * The 'Prompt' effect
  Prompt,
  prompt,
  promptWith,
  promptTools,
  promptPar,
  promptsParallel,
  runPrompt,
  runPromptResultEff,

  -- * Runner configuration
  PromptConfig (..),
  defaultPromptConfig,

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
import Data.Functor ((<&>))
import Data.Proxy (Proxy (..))

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, Value, eitherDecodeStrictText)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (toStrict)

-- openapi3
import Data.OpenApi (ToSchema)

-- universe-base
import Data.Universe.Class (universe)

-- effectful
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, IOE, runEff, (:>))
import Effectful.Concurrent.Async (Concurrent, concurrently, mapConcurrently, runConcurrent)
import Effectful.Dispatch.Dynamic (interpret, localSeqUnlift, send)
import Effectful.Error.Static (Error, catchError, runErrorNoCallStack, throwError)
import Effectful.State.Static.Local (State, evalState, get, modify, put)

-- shroom
import Control.Monad.Prompt.Backend (
  Backend (..),
  BackendError (..),
  BackendReply (..),
  ContextItem (..),
  ToolCall (..),
  ToolResult (..),
 )
import Control.Monad.Prompt.Schema (ToolDef, ToolDispatcher, schemaWithDefs)
import Control.Monad.Prompt.Tool (ToolBinding (..), dispatchBindings)
import Data.Shroom.Class (Surveyable, describeProperties, description, propertyHolds)

-- * The Prompt effect

{- | Dynamic effect: request a typed value from the model, using whatever
context is currently held in "Effectful.State.Static.Local".
-}
data Prompt :: Effect where
  RequestPrompt :: (Surveyable a, ToSchema a, FromJSON a) => Prompt m a
  RequestPromptTools ::
    (Surveyable a, ToSchema a, FromJSON a) =>
    [ToolBinding m] ->
    Prompt m a

type instance DispatchOf Prompt = Dynamic

-- | Request a value of type @a@ from the LLM. Mirrors 'Control.Monad.Prompt.Promptable.prompt'.
prompt :: forall a es. (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) => Eff es a
prompt = send RequestPrompt

-- | Like 'prompt', with an extra piece of context scoped to this request only.
promptWith ::
  forall a es.
  (Prompt :> es, State [ContextItem] :> es, Surveyable a, ToSchema a, FromJSON a) =>
  Text ->
  Eff es a
promptWith txt = withContext txt (prompt @a)

{- | Like 'prompt', but the model may call any of the given tools. Build each
'ToolBinding' with 'Control.Monad.Prompt.Tool.toolBinding' — that is only
possible when the tool's 'Control.Monad.Prompt.Tool.Tool' effect is in
@es@, which is the compile-time denial: a program cannot offer a tool it
was not itself given.
-}
promptTools ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  [ToolBinding (Eff es)] ->
  Eff es a
promptTools bindings = send (RequestPromptTools bindings)

{- | Run two programs concurrently via "Effectful.Concurrent.Async", returning
both results. Parallelism is an explicit call here, rather than hiding
inside an @Applicative@\'s @('<*>')@.
-}
promptPar :: (Concurrent :> es) => Eff es a -> Eff es b -> Eff es (a, b)
promptPar = concurrently

-- | Run a list of programs concurrently, returning all results.
promptsParallel :: (Concurrent :> es) => [Eff es a] -> Eff es [a]
promptsParallel = mapConcurrently id

-- * Runner configuration

-- | Model-independent configuration for 'runPrompt' \/ 'runPromptResultEff'.
data PromptConfig = PromptConfig
  { maxRetries :: Int
  {- ^ Maximum number of re-prompts when a parsed value fails property
  validation, on a 'Control.Monad.Prompt.Backend.BackendTruncated' reply, or
  on a 'Control.Monad.Prompt.Backend.BackendTransportError'.
  Default: 3.
  -}
  , maxToolSteps :: Maybe Int
  {- ^ Maximum number of tool-call iterations per 'promptTools' call before
  giving up. 'Nothing' means no limit. Default: 10.
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

-- * Context

-- | Append a 'ContextItem' for the remainder of the current computation.
addContextItem :: (State [ContextItem] :> es) => ContextItem -> Eff es ()
addContextItem item = modify (\ctx -> ctx <> [item] :: [ContextItem])

-- | Append a piece of user-turn text for the remainder of the current computation.
context :: (State [ContextItem] :> es) => Text -> Eff es ()
context = addContextItem . UserMessage

{- | Scope a 'ContextItem' to a sub-computation: visible only within @act@, and
not carried past it — including anything @act@ itself added.
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
the right computation from there.
-}
orElse :: (Error Text :> es, State [ContextItem] :> es) => Eff es a -> Eff es a -> Eff es a
orElse l r = do
  (saved :: [ContextItem]) <- get
  l `catchError` \_ (_ :: Text) -> put saved >> r

-- * Interpreter

{- | Interpret 'Prompt' against a 'Backend', threading context through
"Effectful.State.Static.Local" and surfacing failure via
"Effectful.Error.Static". On a parse failure, a property violation or a
'BackendTransportError' the request is retried up to 'maxRetries' times,
each time appending the offending response (as an 'AssistantMessage') and a
'UserMessage' naming what went wrong. A 'BackendTruncated' reply is retried
the same way, but appends only a 'UserMessage' saying the reply was cut off
at the length limit — the half-finished text is not replayed. A 'BackendRefusal' is terminal and
consumes no retry budget — the model declining outright is not something a
re-prompt fixes. 'debugLog' emits the same before\/after-call lines with the
same OK \/ PARSE FAIL \/ VALIDATION FAIL \/ ERROR outcomes as before.

'RequestPrompt' passes no tools. 'RequestPromptTools' turns its
@['ToolBinding']@ into the @['ToolDef']@ \/ dispatcher pair 'toolLoop'
drives — via 'localSeqUnlift', since a binding's call runs in the caller's
local effect stack.
-}
runPrompt ::
  forall es a.
  (IOE :> es, Error Text :> es, State [ContextItem] :> es) =>
  Backend (Eff es) ->
  PromptConfig ->
  Eff (Prompt : es) a ->
  Eff es a
runPrompt backend promptCfg = interpret $ \env (request :: Prompt (Eff localEs) b) -> case request of
  -- No tools for 'RequestPrompt' (unchanged from before todo 10).
  RequestPrompt -> runRequest [] (\_ _ -> pure (Left "no tools")) (Just 0)
  -- 'RequestPromptTools': build the dispatcher out of the bindings by
  -- unlifting each binding's call — which runs in the request's own local
  -- effect stack ('localEs') — into this interpreter's 'es'.
  RequestPromptTools bindings ->
    runRequest
      (fmap toolBindingDef bindings)
      (\name v -> localSeqUnlift env (\unlift -> unlift (dispatchBindings bindings name v)))
      (maxToolSteps promptCfg)
  where
    runRequest ::
      forall b.
      (Surveyable b, ToSchema b, FromJSON b) =>
      [ToolDef] ->
      ToolDispatcher (Eff es) ->
      Maybe Int ->
      Eff es b
    runRequest toolDefs dispatch maxSteps = do
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
      (a, rawTxt) <- attemptLoop toolDefs dispatch maxSteps ctx typeDesc schema checkProps (maxRetries promptCfg) (maxRetries promptCfg)
      put (ctx <> [AssistantMessage rawTxt])
      pure a

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
      [ToolDef] ->
      ToolDispatcher (Eff es) ->
      Maybe Int ->
      [ContextItem] ->
      Text ->
      Value ->
      (c -> [Text]) ->
      Int ->
      Int ->
      Eff es (c, Text)
    attemptLoop toolDefs dispatch maxSteps ctx typeDesc schema checkProps totalRetries retriesLeft = do
      let attemptNum = totalRetries - retriesLeft + 1
          separator = T.replicate 60 "─"
          renderItem (SystemMessage t) = "[system]    " <> t
          renderItem (UserMessage t) = "[user]      " <> t
          renderItem (AssistantMessage t) = "[assistant] " <> t
          renderItem (ToolCallMessage calls) = "[tool call] " <> T.intercalate ", " (fmap toolCallName calls)
          renderItem (ToolResultMessage results) = "[tool result] " <> T.intercalate ", " (fmap toolResultName results)
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
      result <- toolLoop toolDefs dispatch maxSteps ctx typeDesc schema
      let attemptsStr = "(" <> pack (show attemptNum) <> " attempt(s))"
          retry = attemptLoop toolDefs dispatch maxSteps
      case result of
        Left (BackendRefusal {refusalMessage}) -> do
          -- Terminal: a refusal is not something a re-prompt can fix.
          logDebug $ "✗ REFUSAL: " <> refusalMessage
          throwError $ refusalMessage <> "\n" <> attemptsStr
        Left (BackendTruncated partial) -> do
          -- Retried, but with a message that says what went wrong: a blind
          -- re-prompt would get the same overlong answer. The cut-off text
          -- stays out of the context; it is half a document.
          logDebug $ "✗ TRUNCATED: " <> partial
          if retriesLeft <= 0
            then throwError $ "The reply was cut off at the length limit.\n" <> attemptsStr
            else do
              let retryCtx =
                    ctx
                      <> [ UserMessage $
                             "IMPORTANT: Your previous response was cut off at the length limit, so it was incomplete.\n"
                               <> "Generate a new, shorter JSON response that matches the required schema exactly."
                         ]
              retry retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)
        Left (BackendTransportError err) -> do
          logDebug $ "✗ ERROR: " <> err
          if retriesLeft <= 0
            then throwError $ err <> "\n" <> attemptsStr
            else retry ctx typeDesc schema checkProps totalRetries (retriesLeft - 1)
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
                  retry retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)
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
                          retry retryCtx typeDesc schema checkProps totalRetries (retriesLeft - 1)

    eitherDecodeStrictText' :: forall c. (FromJSON c) => Text -> Either Text c
    eitherDecodeStrictText' t = case eitherDecodeStrictText t of
      Left err -> Left (mconcat ["JSON decode error: ", t, "\n", pack err])
      Right v -> Right v

    -- \| Drive one prompt request's worth of tool calling against 'backend',
    --    one 'runBackendChat' call at a time, until it answers or the step
    --    budget is spent. Typed concretely over @['ContextItem']@\/'BackendReply'
    --    rather than a backend-native @ctx@\/@resp@ pair.
    --
    --    Budget rules:
    --    a round costs one step only when at least one call succeeds, failed
    --    calls are free, each successful result is annotated with the remaining
    --    count, and exhaustion sends one final tool-free call so the model can
    --    still answer.
    --
    toolLoop ::
      [ToolDef] ->
      ToolDispatcher (Eff es) ->
      -- \| Steps remaining (@Nothing@ = unlimited)
      Maybe Int ->
      [ContextItem] ->
      Text ->
      Value ->
      Eff es (Either BackendError Text)
    toolLoop toolDefs dispatch stepsLeft ctx typeDesc schema = do
      result <- runBackendChat backend ctx typeDesc schema toolDefs
      case result of
        Left err -> pure (Left err)
        Right (BackendAnswer txt) -> pure (Right txt)
        Right (BackendToolCalls calls) ->
          case stepsLeft of
            -- Budget exhausted: inform the model, then call without tools.
            Just 0 -> do
              logDebug "[tool budget exhausted]"
              let exhausted = "Tool budget exhausted. No further tool calls will be processed. Please give your final answer using the information already available."
                  fakeResults = [ToolResult {toolResultId = tc.toolCallId, toolResultName = tc.toolCallName, toolResultOutcome = Right exhausted} | tc <- calls]
                  ctx' = ctx <> [ToolCallMessage calls, ToolResultMessage fakeResults]
              final <- runBackendChat backend ctx' typeDesc schema []
              pure $ case final of
                Left err -> Left err
                Right (BackendAnswer txt) -> Right txt
                Right (BackendToolCalls _) -> Left (BackendTransportError "backend requested tools on the tool-free exhaustion call")
            _ -> do
              tagged <-
                traverse
                  ( \tc -> do
                      logDebug $ ">>> TOOL CALL: " <> tc.toolCallName <> " " <> toStrict (encodeToLazyText tc.toolCallArguments)
                      r <- dispatch tc.toolCallName tc.toolCallArguments
                      logDebug $ "<<< TOOL RESULT: " <> tc.toolCallName <> " " <> either ("ERROR: " <>) ("OK: " <>) r
                      pure (tc, r)
                  )
                  calls
              -- Count successes; only successful calls cost budget.
              let successCount = length [() | (_, Right _) <- tagged]
                  newStepsLeft = stepsLeft <&> \n -> n - (if successCount > 0 then 1 else 0)
                  -- Annotate each successful result with remaining budget.
                  annotate (tc, Right txt) =
                    let note = case newStepsLeft of
                          Nothing -> ""
                          Just n -> "\n[" <> T.pack (show n) <> " tool step(s) remaining]"
                     in ToolResult {toolResultId = tc.toolCallId, toolResultName = tc.toolCallName, toolResultOutcome = Right (txt <> note)}
                  annotate (tc, Left err) =
                    ToolResult {toolResultId = tc.toolCallId, toolResultName = tc.toolCallName, toolResultOutcome = Left err}
                  results = fmap annotate tagged
                  ctx' = ctx <> [ToolCallMessage calls, ToolResultMessage results]
              toolLoop toolDefs dispatch newStepsLeft ctx' typeDesc schema

{- | Run a 'Prompt' program end to end against a 'Backend': interpret
'Prompt', thread context via "Effectful.State.Static.Local" starting from an
empty history, catch failure into an 'Either', and provide
"Effectful.Concurrent.Async" so 'promptPar' \/ 'promptsParallel' branches
run concurrently.

The 'Backend' is a value, not a typeclass instance — build one with e.g.
@shroom-baikai@\'s @Control.Monad.Prompt.Baikai.claudeBackend@ or
'Control.Monad.Prompt.FileMock.fileMockBackend', applied to your config.
Those builders are themselves polymorphic in @m@ (given @'MonadIO' m@), so
passing one straight to this function instantiates it at the effect stack
below without any extra annotation.
-}
runPromptResultEff ::
  Backend (Eff '[State [ContextItem], Error Text, Concurrent, IOE]) ->
  PromptConfig ->
  Eff '[Prompt, State [ContextItem], Error Text, Concurrent, IOE] a ->
  IO (Either Text a)
runPromptResultEff backend promptCfg program =
  runEff
    . runConcurrent
    . runErrorNoCallStack
    . evalState ([] :: [ContextItem])
    $ runPrompt backend promptCfg program
