{- | Tool use support for 'PromptT'.

Define tools as Haskell types, provide handlers, and pass them to
'runPromptT'.  The LLM backend will call tools automatically during
'prompt' \/ 'promptWith' if the model requests them.

@
-- | A web search query.
data MySearch = MySearch { query :: Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

\$(deriveDescribable ''MySearch)
instance Surveyable MySearch
instance Promptable MySearch

instance Toolable MySearch where
  toolDescription _ = Just "Returns results from the search API."

mySearchHandler :: ToolHandler MySearch
mySearchHandler = ToolHandler $ \\(MySearch q) -> callSearchAPI q

result <- runPromptResultTWith cfg $
  runPromptT defaultPromptConfig (mySearchHandler :* Nil) myProgram
@
-}
module Control.Monad.Prompt.Tool (module Control.Monad.Prompt.Tool) where

-- base
import Control.Monad.IO.Class (MonadIO (..))
import Data.Char (isUpper, toLower)
import Data.List (intercalate)
import Data.Proxy (Proxy (..))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, Result (..), Value (..), fromJSON)
import Data.Aeson.Text (encodeToLazyText)

-- text
import Data.Text.Lazy (toStrict)

-- sop-core
import Data.SOP (All, K (..), NP (..), SListI, hcmap, hcollapse)
import Data.SOP.NP ()

-- SListI instances

-- shroom

import Control.Monad.Prompt.Schema (ToolDef (..), schemaWithDefs)
import Data.Shroom.Class (Promptable, describeType)

import Data.Functor ((<&>))

-- * Tool typeclass

{- | A type whose values the LLM can supply as tool inputs.

The LLM receives the tool name (derived from the type name in snake_case),
a description (from 'describeType', optionally extended by 'toolDescription'),
and the JSON schema of @t@.  When the model requests a tool call, the runner
parses the input as @t@, calls the 'ToolHandler', and feeds the 'Text'
result back before continuing.

Minimal complete definition: none — all methods have defaults.
-}
class (Promptable t, FromJSON t, Typeable t) => Toolable t where
  {- | Additional description appended to 'describeType' when sending tool
  metadata to the LLM.  Use this for operational details not obvious from
  the type (e.g. rate limits, return format).  'Nothing' means no extra text.
  -}
  toolDescription :: Proxy t -> Maybe Text
  toolDescription _ = Nothing

{- | Derive the tool name from the type name: convert @CamelCase@ to
@snake_case@, e.g. @WebSearch@ → @"web_search"@.
-}
toolName :: forall t. (Typeable t) => Proxy t -> Text
toolName p =
  let name = tyConName (typeRepTyCon (typeRep p))
   in T.pack $ fmap toLower $ intercalate "_" $ splitCamel name
  where
    splitCamel [] = []
    splitCamel (c : cs) =
      let (word, rest) = break isUpper cs
       in (c : word) : splitCamel rest

-- * Tool handler

{- | A handler for tool @t@: receives the parsed input and returns either a
failure message (@Left@) or a success result (@Right@) fed back to the LLM.
Failures are reported to the model but do __not__ consume step budget.
-}
newtype ToolHandler t = ToolHandler
  { runToolHandler :: t -> IO (Either Text Text)
  }

-- * Dispatcher

{- | A runtime dispatcher: given @(tool_name, input_value)@ returns the
tool result or an error.
-}
type ToolDispatcher m = Text -> Value -> m (Either Text Text)

{- | Build a 'ToolDispatcher' from a heterogeneous list of 'ToolHandler's.
Walks the list by tool name; returns @Left@ for unknown tools (with the
list of available tool names) or parse errors, @Right@ on success.
-}
makeDispatcher ::
  forall tools m.
  (SListI tools, All Toolable tools, MonadIO m) =>
  NP ToolHandler tools ->
  ToolDispatcher m
makeDispatcher handlers name input = go handlers name input
  where
    allNames :: [Text]
    allNames = hcollapse $ hcmap (Proxy @Toolable) extractName handlers
      where
        extractName :: forall t. (Toolable t) => ToolHandler t -> K Text t
        extractName _ = K (toolName (Proxy @t))

    go :: forall ts. (All Toolable ts) => NP ToolHandler ts -> ToolDispatcher m
    go Nil n _ =
      pure $ Left ("Unknown tool: " <> n <> ". Available tools: " <> T.intercalate ", " allNames)
    go (h :* rest) n inp
      | toolName (proxyOf h) == n =
          case fromJSON inp of
            Error e -> pure $ Left ("Tool input parse error for " <> n <> ": " <> T.pack e)
            Success t -> liftIO (runToolHandler h t)
      | otherwise = go rest n inp

    proxyOf :: forall t. ToolHandler t -> Proxy t
    proxyOf _ = Proxy

{- | Extract 'ToolDef' values for all tools in an 'NP'.
Passed to backends so they can register the tools with the LLM API.
-}
toolDefsRaw ::
  forall tools.
  (SListI tools, All Toolable tools) =>
  NP ToolHandler tools ->
  [ToolDef]
toolDefsRaw = hcollapse . hcmap (Proxy @Toolable) extract
  where
    extract :: forall t. (Toolable t) => ToolHandler t -> K ToolDef t
    extract _ =
      K
        ToolDef
          { toolDefName = toolName p
          , toolDefDescription = describeType p <> maybe "" (" " <>) (toolDescription p)
          , toolDefSchema = schemaWithDefs p
          }
      where
        p = Proxy @t

-- * Generic tool loop

{- | Backend-specific operations for the generic tool loop.
@ctx@ is the backend's native message list type.
@resp@ is the backend's native response type.
-}
data ToolLoopOps m ctx resp = ToolLoopOps
  { callModel :: ctx -> m (Either Text resp)
  -- ^ Send the current message list; return error or response.
  , callModelNoTools :: ctx -> m (Either Text resp)
  {- ^ Send the current message list without offering any tools.
  Called once when the step budget is exhausted so the model can
  produce a final answer with whatever information it already has.
  -}
  , detectTools :: resp -> Maybe [(Text, Text, Value)]
  {- ^ Extract tool calls: 'Nothing' = no tools called,
  @'Just' [(tool_use_id, tool_name, input_value)]@ = tool calls requested.
  -}
  , appendExchange :: ctx -> resp -> [(Text, Text, Either Text Text)] -> ctx
  {- ^ Extend the context after a tool exchange.
  Arguments: current ctx, the assistant response, list of
  @(tool_use_id, tool_name, result_or_error)@.
  Must append the assistant turn and the tool result turn(s).
  -}
  , extractText :: resp -> Text
  -- ^ Extract the final response text when no tools are called.
  , logEvent :: Text -> m ()
  {- ^ Called for each tool call, tool result, and budget-exhaustion event.
  Use @const (pure ())@ to disable logging.
  -}
  }

{- | Generic tool loop used by backends.

Calls 'callModel', checks for tool calls, dispatches them, and loops
until the model returns a non-tool response or the step limit is reached.

Budget rules:

* A round costs 1 step only when at least one tool call succeeds (@Right@).
* Failed tool calls (@Left@ from the dispatcher) are free — the model is
  informed of the failure but the budget is not decremented.
* Each successful result is annotated with the remaining step count.
* When the budget reaches zero and the model still requests tools, every
  pending call receives a "budget exhausted" message and 'callModelNoTools'
  is invoked once so the model can produce a final answer.
-}
genericToolLoop ::
  (Monad m) =>
  ToolLoopOps m ctx resp ->
  ToolDispatcher m ->
  ctx ->
  -- | Steps remaining (@Nothing@ = unlimited)
  Maybe Int ->
  m (Either Text Text)
genericToolLoop ops dispatch ctx stepsLeft = do
  result <- ops.callModel ctx
  case result of
    Left err -> pure (Left err)
    Right resp ->
      case ops.detectTools resp of
        Nothing -> pure (Right (ops.extractText resp))
        Just calls ->
          case stepsLeft of
            -- Budget exhausted: inform the model, then call without tools.
            Just 0 -> do
              ops.logEvent "[tool budget exhausted]"
              let exhausted = "Tool budget exhausted. No further tool calls will be processed. Please give your final answer using the information already available."
                  fakeResults = [(uid, name, Right exhausted) | (uid, name, _) <- calls]
                  ctx' = ops.appendExchange ctx resp fakeResults
              ops.callModelNoTools ctx' >>= \case
                Left err -> pure (Left err)
                Right resp' -> pure (Right (ops.extractText resp'))
            _ -> do
              tagged <-
                traverse
                  ( \(uid, name, input) -> do
                      ops.logEvent $ ">>> TOOL CALL: " <> name <> " " <> toStrict (encodeToLazyText input)
                      r <- dispatch name input
                      ops.logEvent $ "<<< TOOL RESULT: " <> name <> " " <> either ("ERROR: " <>) ("OK: " <>) r
                      pure (uid, name, r)
                  )
                  calls
              -- Count successes; only successful calls cost budget.
              let successCount = length [() | (_, _, Right _) <- tagged]
                  newStepsLeft = stepsLeft <&> \n -> n - (if successCount > 0 then 1 else 0)
                  -- Annotate each successful result with remaining budget.
                  annotate (uid, name, Right txt) =
                    let note = case newStepsLeft of
                          Nothing -> ""
                          Just n -> "\n[" <> T.pack (show n) <> " tool step(s) remaining]"
                     in (uid, name, Right (txt <> note))
                  annotate other = other
                  taggedAnnotated = fmap annotate tagged
                  ctx' = ops.appendExchange ctx resp taggedAnnotated
              genericToolLoop ops dispatch ctx' newStepsLeft
