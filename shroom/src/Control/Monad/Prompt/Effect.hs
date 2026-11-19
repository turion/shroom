{- |
Module      : Control.Monad.Prompt.Effect
Description : Programs that prompt a language model, built from @effectful@ effects.

A shroom program is an @'Eff' es@ computation with 'Prompt' in @es@, where
'Eff' is the monad of the <https://hackage.haskell.org/package/effectful effectful>
package. Context is plain state ('addContextItem', 'withContext'), failure is an
error ('failWith', 'orElse') and parallel requests use "Effectful.Concurrent.Async"
('promptPar'). 'runPromptResultEff' runs a program against a t'Backend'.

In ordinary code, request a value with 'Control.Monad.Prompt.Promptable.prompt',
which follows the type's 'Control.Monad.Prompt.Promptable.Promptable' strategy.
'promptSingle' is the single request that method defaults to, 'promptTools' offers
the model tools to call, and 'improve' asks it to correct a value.
-}
module Control.Monad.Prompt.Effect (
  -- * Requesting values
  Prompt,
  promptSingle,
  promptSingleWith,
  promptTools,
  improve,

  -- * Running a program
  runPromptResultEff,
  runPrompt,
  PromptConfig (..),
  defaultPromptConfig,

  -- * Context
  addContextItem,
  context,
  withContextItem,
  withContext,

  -- * Failure and fallback
  failWith,
  orElse,

  -- * Parallel requests
  promptPar,
  promptsParallel,
) where

-- base
import Control.Monad.IO.Class (liftIO)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON (..), Value, eitherDecodeStrictText, object, withObject, (.:), (.=))

-- openapi3
import Data.OpenApi (ToSchema)

-- universe-base
import Data.Universe.Class (universe)

-- effectful
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, IOE, runEff, (:>))
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
import Control.Monad.Prompt.Schema (ToolDef, ToolDispatcher, encodeJson, schemaWithDefs)
import Control.Monad.Prompt.Tool (ToolBinding (..), dispatchBindings)
import Data.Shroom.Class (Surveyable, describeProperties, description, propertyHolds)

-- * The Prompt effect

{- | The effect signature of a shroom program: the operations it can perform
against a language model. Put @Prompt ':>' es@ in a program's constraints to let
it prompt. An interpreter such as 'runPrompt' handles the operations, with the
conversation held in the @'State' ['ContextItem']@ effect.

The signature offers three operations:

* Request a value of a type, sent by 'promptSingle' (and so by
  'Control.Monad.Prompt.Promptable.prompt', through its default).
* Request a value while the model may call tools, sent by 'promptTools'.
* Improve a value, sent by 'improve'.

For example, a program that requests a @User@ and lets the model correct it:

@
refinedUser :: ('Prompt' ':>' es) => 'Eff' es User
refinedUser = do
  user <- 'promptSingle'
  fromMaybe user \<$> 'improve' user
@
-}
data Prompt (m :: Type -> Type) (result :: Type) where
  RequestPrompt :: (Surveyable a, ToSchema a, FromJSON a) => Prompt m a
  RequestPromptTools ::
    (Surveyable a, ToSchema a, FromJSON a) =>
    [ToolBinding m] ->
    Prompt m a
  RequestImprove :: (Surveyable a, ToSchema a, FromJSON a) => a -> Prompt m (Maybe a)

type instance DispatchOf Prompt = Dynamic

{- | Request a value of type @a@ in a single request, one call to the backend for
the whole value: the prompt text is the type's 'description' and the schema its
'ToSchema'.

Note that this skips the type's 'Control.Monad.Prompt.Promptable.Promptable'
strategy, such as a @deriving via Generically@. It is the default of
'Control.Monad.Prompt.Promptable.prompt', which honours the strategy and is the
usual choice; call 'promptSingle' directly inside a strategy itself.
-}
promptSingle :: forall a es. (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) => Eff es a
promptSingle = send RequestPrompt

{- | Like 'promptSingle', with an extra piece of context scoped to this request.
The strategy-aware counterpart is 'withContext' around 'Control.Monad.Prompt.Promptable.prompt'.
-}
promptSingleWith ::
  forall a es.
  (Prompt :> es, State [ContextItem] :> es, Surveyable a, ToSchema a, FromJSON a) =>
  -- | The context, a piece of user-turn text that this request sees and no other.
  Text ->
  Eff es a
promptSingleWith txt = withContext txt (promptSingle @a)

{- | Like 'promptSingle', but the model may call tools. 'maxToolSteps' limits the
number of tool rounds. See "Control.Monad.Prompt.Tool" for how to define and run tools.
-}
promptTools ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  {- | The tools the model may call. Build each t'ToolBinding' with
  'Control.Monad.Prompt.Tool.toolBinding', which needs the tool's
  'Control.Monad.Prompt.Tool.Tool' effect in @es@: a program cannot offer a tool it
  was not given.
  -}
  [ToolBinding (Eff es)] ->
  Eff es a
promptTools bindings = send (RequestPromptTools bindings)

{- | Ask the model to improve a value it (or anyone) has already produced:
'Nothing' means the given value is a good value of its type and the model is
content with it, @'Just' v'@ means the model returned a corrected whole value
@v'@.

As for 'promptSingle', the reply is appended to the context as an 'AssistantMessage'.

The reply goes through the same retry path as any request. An @improved@ value
that fails to parse or violates a property is re-prompted; so is a @fine@
verdict when the value passed in violates a property. So 'Nothing' always means
that the original value satisfies every 'Data.Shroom.Class.Property' of @a@.
Only the properties 'Surveyable' checks in code are enforced, not informal ones
that only the model can judge.

Like 'promptSingle', 'improve' is a single request: it does not call the type's own
'Control.Monad.Prompt.Promptable.prompt', so a prompt strategy may call it
without recursing into itself.
-}
improve ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  -- | The value to improve. The model is shown it for this request only.
  a ->
  Eff es (Maybe a)
improve = send . RequestImprove

{- | The reply to an 'improve' request: the model's verdict on the value shown.
Internal; the wire form is one object, see 'improveSchema'.
-}
data Verdict a = VerdictFine | VerdictImproved a

{- | A @fine@ verdict ignores @value@ (present on the wire, see 'improveSchema',
but never parsed, so a @fine@ reply without one is accepted too); an
@improved@ one requires it.
-}
instance (FromJSON a) => FromJSON (Verdict a) where
  parseJSON = withObject "Verdict" $ \o -> do
    verdict <- o .: "verdict"
    case (verdict :: Text) of
      "fine" -> pure VerdictFine
      "improved" -> VerdictImproved <$> o .: "value"
      other -> fail ("verdict must be \"fine\" or \"improved\", not " <> show other)

{- | The wire schema of a 'Verdict' over @a@: a single object whose root is
built by hand, so that it is not an openapi3-derived sum (which would emit a
@oneOf@ at the root, which structured-output schemas handle unevenly).
@value@ is the (already flattened) schema of @a@ itself. Both @verdict@ and
@value@ are listed in @required@: strict structured-output modes (OpenAI's
@json_schema@ with @strict@) reject an object whose @properties@ are not all
required, and JSON Schema cannot make @value@ conditional on the verdict
without @if@\/@oneOf@. So the model always sends a @value@, and for @fine@ the
parser ignores it (see the 'FromJSON' instance of 'Verdict', which still accepts
a @fine@ reply without one, for backends that do not enforce @required@).
-}
improveSchema :: forall a. (ToSchema a) => Proxy a -> Value
improveSchema prx =
  object
    [ "type" .= ("object" :: Text)
    , "properties"
        .= object
          [ "verdict"
              .= object
                [ "type" .= ("string" :: Text)
                , "enum" .= (["fine", "improved"] :: [Text])
                , "description" .= ("\"fine\" if the given value is already good (then repeat it unchanged in \"value\"), \"improved\" if a corrected value follows in \"value\"." :: Text)
                ]
          , "value" .= schemaWithDefs prx
          ]
    , "required" .= (["verdict", "value"] :: [Text])
    , "additionalProperties" .= False
    ]

-- | The descriptions of the properties of @a@ that a value does not satisfy.
violatedProperties :: forall a. (Surveyable a) => a -> [Text]
violatedProperties v =
  [ desc
  | p <- universe
  , not (propertyHolds v p)
  , Just desc <- [describeProperties (Proxy @a) p]
  ]

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
  {- ^ Maximum number of re-prompts per request. A reply that fails to parse,
  a parsed value that violates a property, a 'BackendTruncated' reply and a
  'BackendTransportError' each spend one; a 'BackendRefusal' is terminal and
  spends none. Default: 3.
  -}
  , maxToolSteps :: Maybe Int
  {- ^ Maximum number of tool rounds per 'promptTools' call; 'Nothing' means no
  limit. A round costs one step only when at least one of its calls succeeds.
  Once the steps are spent, the model gets one last call without tools to answer
  with what it already has. Default: 10.
  -}
  , debugLog :: Maybe (Text -> IO ())
  {- ^ Optional logger called before each LLM call and after each response.
  Receives a human-readable 'Text' summary. Pass @Just putStrLn@, from
  "Data.Text.IO", for stdout, or @tasty-hunit@\'s @step@ callback in tests.
  Default: 'Nothing'.
  -}
  }

-- | Default t'PromptConfig': up to 3 retries, up to 10 tool steps, no debug logging.
defaultPromptConfig :: PromptConfig
defaultPromptConfig = PromptConfig {maxRetries = 3, maxToolSteps = Just 10, debugLog = Nothing}

-- * Context

-- | Append a 'ContextItem' for the remainder of the current computation.
addContextItem :: (State [ContextItem] :> es) => ContextItem -> Eff es ()
addContextItem item = modify (\ctx -> ctx <> [item] :: [ContextItem])

-- | Append a piece of user-turn text for the remainder of the current computation.
context :: (State [ContextItem] :> es) => Text -> Eff es ()
context = addContextItem . UserMessage

{- | Scope a 'ContextItem' to a computation: visible only within the given
computation, and not carried past it, including anything that computation itself
added.
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

{- | Try one computation and, on failure, run another. The context is restored to
how it stood before the attempt, discarding anything the failing computation added,
and the other computation runs from there.
-}
orElse ::
  (Error Text :> es, State [ContextItem] :> es) =>
  -- | The computation to try first.
  Eff es a ->
  -- | The computation to run if the first fails.
  Eff es a ->
  Eff es a
orElse l r = do
  (saved :: [ContextItem]) <- get
  l `catchError` \_ (_ :: Text) -> put saved >> r

-- * Interpreter

{- | Interpret 'Prompt' against a t'Backend', threading context through
"Effectful.State.Static.Local" and surfacing failure via
"Effectful.Error.Static".

A request is retried up to 'maxRetries' times. After a parse failure or a
property violation, the retry appends the offending response (as an
'AssistantMessage') and a 'UserMessage' naming what went wrong. After a
'BackendTruncated' reply it appends only a 'UserMessage' saying the reply was
cut off, and a 'BackendTransportError' is retried with the context unchanged.
A 'BackendRefusal' is terminal and consumes no retry.

The t'Backend' makes one raw call at a time, so this function runs the tool
cycle of 'promptTools' itself: it dispatches the calls the model requests,
appends their results to the context and calls the t'Backend' again, within
'maxToolSteps'. 'debugLog' reports each call and its outcome.
-}
runPrompt ::
  forall es a.
  (IOE :> es, Error Text :> es, State [ContextItem] :> es) =>
  Backend (Eff es) ->
  PromptConfig ->
  Eff (Prompt : es) a ->
  Eff es a
runPrompt backend promptCfg = interpret $ \env (request :: Prompt (Eff localEs) b) -> case request of
  -- No tools for 'RequestPrompt'.
  RequestPrompt -> let (toolDefs, dispatch, maxSteps) = noTools in runRequest toolDefs dispatch maxSteps
  -- 'RequestPromptTools': build the dispatcher out of the bindings by
  -- unlifting each binding's call — which runs in the request's own local
  -- effect stack ('localEs') — into this interpreter's 'es'.
  RequestPromptTools bindings ->
    runRequest
      (fmap toolBindingDef bindings)
      (\name v -> localSeqUnlift env (\unlift -> unlift (dispatchBindings bindings name v)))
      (maxToolSteps promptCfg)
  RequestImprove original -> runImprove original
  where
    -- The tool setup of a single request: no tool definitions, a dispatcher
    -- that refuses every call, and zero tool steps. Shared by 'RequestPrompt'
    -- and 'RequestImprove'.
    noTools :: ([ToolDef], ToolDispatcher (Eff es), Maybe Int)
    noTools = ([], \_ _ -> pure (Left "no tools"), Just 0)

    -- 'RequestImprove': a single request, no tools, whose reply is a 'Verdict'.
    -- The scoped 'UserMessage' showing the value goes onto the context the
    -- backend sees but not the one left behind; only the raw reply is kept.
    -- A @fine@ verdict is judged by the properties of the value passed in,
    -- an @improved@ one by the properties of the value it carries.
    runImprove ::
      forall b.
      (Surveyable b, ToSchema b, FromJSON b) =>
      b ->
      Eff es (Maybe b)
    runImprove original = do
      ctx <- get
      let prx = Proxy @b
          shown = encodeJson original
          ask =
            UserMessage $
              "Here is a value of the type described below, as JSON:\n"
                <> shown
                <> "\n\nIs it a good value of that type, and does it satisfy every listed property? "
                <> "If it does, answer with verdict \"fine\" and return the value unchanged in \"value\". "
                <> "If it does not, answer with verdict \"improved\" and put a corrected whole value in \"value\"."
          typeDesc =
            description prx
              <> "\nAnswer with a single JSON object: {\"verdict\": \"fine\", \"value\": <the given value, unchanged>} for a good value as it stands, "
              <> "or {\"verdict\": \"improved\", \"value\": <the corrected whole value>}.\n"
          check VerdictFine = violatedProperties original
          check (VerdictImproved v) = violatedProperties v
      let (toolDefs, dispatch, maxSteps) = noTools
      (verdict, rawTxt) <-
        attemptLoop toolDefs dispatch maxSteps (ctx <> [ask]) typeDesc (improveSchema prx) check (maxRetries promptCfg) (maxRetries promptCfg)
      put (ctx <> [AssistantMessage rawTxt])
      pure $ case verdict of
        VerdictFine -> Nothing
        VerdictImproved v -> Just v

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
          checkProps = violatedProperties @b
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
    --    budget is spent.
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
                      logDebug $ ">>> TOOL CALL: " <> tc.toolCallName <> " " <> encodeJson tc.toolCallArguments
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

{- | Run a 'Prompt' program against a t'Backend', starting from an empty context
and returning a failure as 'Left'. 'promptPar' and 'promptsParallel' run
their branches concurrently.

Build the t'Backend' with, for example, the @shroom-baikai@ package's
<https://hackage.haskell.org/package/shroom-baikai/docs/Control-Monad-Prompt-Baikai.html#v:claudeBackend claudeBackend>
or 'Control.Monad.Prompt.FileMock.fileMockBackend'. Both build a @t'Backend' m@ for
any @MonadIO m@, so the result fits here without an annotation.
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
