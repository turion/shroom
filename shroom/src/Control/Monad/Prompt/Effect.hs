{- | shroom's program vocabulary expressed as 'effectful' effects: context,
scoping, failure, fallback, parallelism and typed prompting, each an
ordinary effect rather than a constructor in a bespoke DSL.

== Context, scoping, failure, fallback, parallelism

* Context — held in "Effectful.State.Static.Local" ('addContextItem' \/
  'context')
* Scoping — @local@-style, over that same state ('withContextItem' \/
  'withContext')
* The typed request itself — the 'Prompt' dynamic effect ('promptSingle' \/
  'promptSingleWith' \/ 'promptTools')
* Failure — "Effectful.Error.Static" with 'Text' ('failWith')
* Fallback — 'catchError' plus context restoration ('orElse')
* Parallelism — "Effectful.Concurrent.Async", explicit rather than hiding in
  an @Applicative@ instance ('promptPar' \/ 'promptsParallel')

== Which request to reach for

In ordinary code call the class method 'Control.Monad.Prompt.Promptable.prompt'
(from "Control.Monad.Prompt"), which honours a type's 'Control.Monad.Prompt.Promptable.Promptable'
strategy. 'promptSingle' is the flat request that method defaults to: one
request, never consulting 'Control.Monad.Prompt.Promptable.Promptable'. So
@'withContext' t ('Control.Monad.Prompt.Promptable.prompt' \@a)@ is the
strategy-aware equivalent of @'promptSingleWith' t@.

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
  promptSingle,
  promptSingleChecked,
  promptSingleWith,
  promptTools,
  promptToolsChecked,
  improve,
  improveChecked,
  PropertyCheck (..),
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
import Data.Maybe (catMaybes, isNothing)
import Data.Proxy (Proxy (..))

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON (..), Value, eitherDecodeStrictText, object, withObject, (.:), (.=))

-- openapi3
import Data.OpenApi (ToSchema)

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
import Control.Monad.Prompt.Schema (ToolDef, ToolDispatcher, encodeJson, schemaWithDefs)
import Control.Monad.Prompt.Tool (ToolBinding (..), dispatchBindings)
import Data.Shroom.Class (PropertyCheck (..), Surveyable, description, propertyLines, typeProperties)

-- * The Prompt effect

{- | Dynamic effect: request a typed value from the model, using whatever
context is currently held in "Effectful.State.Static.Local".
-}
data Prompt :: Effect where
  RequestPrompt :: (Surveyable a, ToSchema a, FromJSON a) => [PropertyCheck a] -> Prompt m a
  RequestPromptTools ::
    (Surveyable a, ToSchema a, FromJSON a) =>
    [PropertyCheck a] ->
    [ToolBinding m] ->
    Prompt m a
  RequestImprove :: (Surveyable a, ToSchema a, FromJSON a) => [PropertyCheck a] -> a -> Prompt m (Maybe a)

type instance DispatchOf Prompt = Dynamic

{- | Request a value of type @a@ from the LLM in one flat request: the prompt
text is the type's own 'description', and the schema is the type's own
'ToSchema'. It never consults a 'Control.Monad.Prompt.Promptable.Promptable'
instance, so a type's prompt strategy (a @deriving via Generically@, a
hand-written override) is silently skipped. It is the default of the class
method 'Control.Monad.Prompt.Promptable.prompt', which is what to reach for in
ordinary code: for a type with the default strategy the two coincide, and for
any other 'Control.Monad.Prompt.Promptable.prompt' honours the strategy while
this does not. Reach for 'promptSingle' only when a flat request is the point
(inside a strategy itself, or to compare against it).
-}
promptSingle :: forall a es. (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) => Eff es a
promptSingle = promptSingleChecked []

{- | Like 'promptSingle', with properties of this call's own that a reply must
satisfy in addition to the type's own ('typeProperties').

The type description is exactly the one 'promptSingle' sends, so a backend that
caches it sees the same text on every call. The extra properties go in a message
of their own: one 'UserMessage' after the current context (and after the
type's own request, for 'improveChecked'), headed \"For this request, the
following properties MUST hold in addition to those in the type description:\"
and listing each property's description; it is sent for this request only and is
not kept, so the context left behind is the one before the request plus the
reply. Properties without a description are left out of that message but are
still checked. With none to list, no message is sent.

The check runs over the type's properties followed by these, so a reply that
breaks one is repaired and counted against the same 'maxRetries' budget as a
reply that breaks the type's own.

Like 'promptSingle', this is flat: it never consults a
'Control.Monad.Prompt.Promptable.Promptable' instance, so a type's own prompt
strategy does not see the extra properties.
-}
promptSingleChecked ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  [PropertyCheck a] ->
  Eff es a
promptSingleChecked = send . RequestPrompt

{- | Like 'promptSingle', with an extra piece of context scoped to this request
only. Equally flat: it never consults 'Control.Monad.Prompt.Promptable.Promptable'.
The strategy-aware equivalent is @'withContext' t ('Control.Monad.Prompt.Promptable.prompt' \@a)@.
-}
promptSingleWith ::
  forall a es.
  (Prompt :> es, State [ContextItem] :> es, Surveyable a, ToSchema a, FromJSON a) =>
  Text ->
  Eff es a
promptSingleWith txt = withContext txt (promptSingle @a)

{- | Like 'promptSingle', but the model may call any of the given tools. Build each
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
promptTools = promptToolsChecked []

{- | Like 'promptTools', with properties of this call's own that a reply must
satisfy in addition to the type's own: told to the model and enforced exactly as
'promptSingleChecked' does, and flat in the same way. They are checked on the
final reply only; tool arguments are not validated against them.
-}
promptToolsChecked ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  [PropertyCheck a] ->
  [ToolBinding (Eff es)] ->
  Eff es a
promptToolsChecked extra bindings = send (RequestPromptTools extra bindings)

{- | Ask the model to improve a value it (or anyone) has already produced:
'Nothing' means the given value is a good value of its type and the model is
content with it, @'Just' v'@ means the model returned a corrected whole value
@v'@.

The request scopes one 'UserMessage' onto the current context, showing the
value as JSON and asking whether it is a good value of the type and satisfies
every listed property, and the reply is one verdict object,
@{"verdict": "fine" | "improved", "value": <a>}@, with @value@ always present:
the corrected value for @improved@, the given value unchanged for @fine@ (where
it is ignored; a @fine@ reply that omits it is still accepted). The type's
description (see 'description') goes with it, as for
'promptSingle'. The scoped message is not kept; the raw reply is appended to the
context as an 'AssistantMessage', as for 'promptSingle'.

The reply goes through the same retry path as any request: an @improved@ value
that fails to parse or violates a property is re-prompted. A @fine@ verdict
is accepted only if the value passed in satisfies every 'Data.Shroom.Class.Property'
of @a@; otherwise it is retried in just the same way, the correction message naming
the violated properties. A violated property without a description (see
'Data.Shroom.Class.describeProperties') counts as much as any other; the correction
then says so in one line. So 'Nothing' always means \"the original
value is valid and the model is content with it\".

'improve' is flat by construction: it never calls the type's own
'Control.Monad.Prompt.Promptable.prompt', so a prompt strategy may call it
without recursing into itself. Only the properties 'Surveyable' can check in
code are enforced; informal ones, which only the model can judge, are
tracked as #26.
-}
improve ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  a ->
  Eff es (Maybe a)
improve = improveChecked []

{- | Like 'improve', with properties of this call's own in addition to the
type's: a @fine@ verdict about a value that breaks one, and an @improved@ value
that breaks one, are both retried from the same 'maxRetries' budget as for the
type's own properties. The properties are told to the model as for
'promptSingleChecked', in a message after the question 'improve' scopes; the type
description is unchanged.
-}
improveChecked ::
  forall a es.
  (Prompt :> es, Surveyable a, ToSchema a, FromJSON a) =>
  [PropertyCheck a] ->
  a ->
  Eff es (Maybe a)
improveChecked extra = send . RequestImprove extra

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

-- | The line that stands for every failed property without a description.
undescribedViolation :: Text
undescribedViolation = "A property without a description was violated."

{- | The message that tells the model the properties of one call, if there is a
description to tell: sent after the context for that request only. The wording
reads right wherever a backend puts the type description.
-}
callPropertiesMessage :: [PropertyCheck a] -> [ContextItem]
callPropertiesMessage extra = case propertyLines extra of
  [] -> []
  ls ->
    [ UserMessage $
        T.intercalate "\n" $
          "For this request, the following properties MUST hold in addition to those in the type description:" : ls
    ]

-- | The checks in the list that a value fails, described or not.
failedChecks :: [PropertyCheck a] -> a -> [PropertyCheck a]
failedChecks checks v = [c | c <- checks, not (c.holds v)]

-- | What a failed check leaves for the retry loop: its description, if it has one.
failedDescriptions :: [PropertyCheck a] -> a -> [Maybe Text]
failedDescriptions checks v = fmap (\c -> c.description) (failedChecks checks v)

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
local effect stack. 'RequestImprove' (see 'improve') passes no tools either,
and goes through the same retry path with a verdict schema of its own.
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
  RequestPrompt extra -> let (toolDefs, dispatch, maxSteps) = noTools in runRequest extra toolDefs dispatch maxSteps
  -- 'RequestPromptTools': build the dispatcher out of the bindings by
  -- unlifting each binding's call — which runs in the request's own local
  -- effect stack ('localEs') — into this interpreter's 'es'.
  RequestPromptTools extra bindings ->
    runRequest
      extra
      (fmap toolBindingDef bindings)
      (\name v -> localSeqUnlift env (\unlift -> unlift (dispatchBindings bindings name v)))
      (maxToolSteps promptCfg)
  RequestImprove extra original -> runImprove extra original
  where
    -- The tool setup of a flat request: no tool definitions, a dispatcher
    -- that refuses every call, and zero tool steps. Shared by 'RequestPrompt'
    -- and 'RequestImprove'.
    noTools :: ([ToolDef], ToolDispatcher (Eff es), Maybe Int)
    noTools = ([], \_ _ -> pure (Left "no tools"), Just 0)

    -- 'RequestImprove': a flat request, no tools, whose reply is a 'Verdict'.
    -- The scoped 'UserMessage' showing the value goes onto the context the
    -- backend sees but not the one left behind; only the raw reply is kept.
    -- A @fine@ verdict is judged by the properties of the value passed in,
    -- an @improved@ one by the properties of the value it carries.
    runImprove ::
      forall b.
      (Surveyable b, ToSchema b, FromJSON b) =>
      [PropertyCheck b] ->
      b ->
      Eff es (Maybe b)
    runImprove extra original = do
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
          checks = typeProperties @b <> extra
          check VerdictFine = failedDescriptions checks original
          check (VerdictImproved v) = failedDescriptions checks v
      let (toolDefs, dispatch, maxSteps) = noTools
      (verdict, rawTxt) <-
        attemptLoop toolDefs dispatch maxSteps (ctx <> [ask] <> callPropertiesMessage extra) typeDesc (improveSchema prx) check (maxRetries promptCfg) (maxRetries promptCfg)
      put (ctx <> [AssistantMessage rawTxt])
      pure $ case verdict of
        VerdictFine -> Nothing
        VerdictImproved v -> Just v

    -- The call's own properties (@extra@) go to the model in a scoped message
    -- after the context, which is not kept; the check runs over the type's
    -- properties and @extra@ alike.
    runRequest ::
      forall b.
      (Surveyable b, ToSchema b, FromJSON b) =>
      [PropertyCheck b] ->
      [ToolDef] ->
      ToolDispatcher (Eff es) ->
      Maybe Int ->
      Eff es b
    runRequest extra toolDefs dispatch maxSteps = do
      ctx <- get
      let prx = Proxy @b
          typeDesc = description prx
          schema = schemaWithDefs prx
          checkProps = failedDescriptions (typeProperties @b <> extra)
      (a, rawTxt) <- attemptLoop toolDefs dispatch maxSteps (ctx <> callPropertiesMessage extra) typeDesc schema checkProps (maxRetries promptCfg) (maxRetries promptCfg)
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
      (c -> [Maybe Text]) ->
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
              let failed = checkProps a
                  -- what the model is told: the failed described properties as they
                  -- are worded, then one line if any undescribed property failed
                  failDescs = catMaybes failed <> [undescribedViolation | any isNothing failed]
               in if null failed
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
