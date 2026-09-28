{- | Internal schema utilities shared by 'Control.Monad.Prompt' and
'Control.Monad.Prompt.Tool'.  Not part of the public API.
-}
module Control.Monad.Prompt.Schema (schemaWithDefs, normalizeSchemaForStructuredOutput, inlineSchema, ToolDef (..), ToolDispatcher) where

-- base
import Data.Proxy (Proxy)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM

-- openapi3
import Data.OpenApi (Referenced (..), ToSchema, declareSchemaRef, getReference)
import Data.OpenApi.Declare (runDeclare)

-- | A backend-agnostic tool definition passed to LLM backends.
data ToolDef = ToolDef
  { toolDefName :: Text
  -- ^ Identifier sent to the LLM.  Must match @[a-zA-Z0-9_-]+@.
  , toolDefDescription :: Text
  -- ^ Human-readable description sent alongside the name and schema.
  , toolDefSchema :: Value
  -- ^ OpenAPI JSON schema for the tool's input type.
  }

{- | A runtime tool dispatcher: given @(tool_name, input_value)@ returns a
result text or an error.
-}
type ToolDispatcher m = Text -> Value -> m (Either Text Text)

{- | Build a JSON schema 'Value' for type @a@ that includes all referenced
sub-schemas in a @$defs@ section, using openapi3's 'declareSchemaRef'.
This rewrites the @#\/components\/schemas\/X@ reference form openapi3 emits
by default into the @#\/$defs\/X@ form a structured-output schema uses, via
'normalizeSchemaForStructuredOutput'.
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
            Object rootKm -> normalizeSchemaForStructuredOutput (Object (KM.insert "$defs" (Object defsKm) rootKm))
            _ -> normalizeSchemaForStructuredOutput rootSchema
        _ -> normalizeSchemaForStructuredOutput rootSchema

{- | Normalise a JSON schema 'Value' for use as a structured-output (or
strict tool) schema:

* Adds @"additionalProperties": false@ to every @"type": "object"@ node.
* Rewrites @$ref@ values from @#\/components\/schemas\/X@ (openapi3's
  default) to @#\/$defs\/X@.
* Folds @"minimum"@\/@"maximum"@ on every @"type": "integer"@ node into that
  field's @"description"@, then removes the keywords, rather than dropping
  them. Numeric bounds are not part of the wire schema any provider used
  here accepts, but the official SDKs move them into the description so the
  model still hears the constraint; this does the same rather than silently
  discarding it.

String @"format"@ is left alone: Anthropic's structured outputs accept
@date-time@, @date@, @time@, @duration@, @email@, @hostname@, @uri@,
@ipv4@, @ipv6@ and @uuid@, and stripping it would throw away schema the API
accepts.

Note that a JSON Schema @"format"@ (and the folded bounds above) is an
/annotation/, not a constraint: a live test against a schema declaring
@format: email@ got back @"thompsons sophia\@outlook.com "@, with a stray
space in the middle. Sending it is still right, since it informs the model,
but nothing on the wire enforces it. That is what the @Surveyable@
properties in 'Data.Shroom.Class' are for.
-}
normalizeSchemaForStructuredOutput :: Value -> Value
normalizeSchemaForStructuredOutput (Object km) =
  let km1 = case KM.lookup "$ref" km of
        Just (String ref) -> KM.insert "$ref" (String (T.replace "#/components/schemas/" "#/$defs/" ref)) km
        _ -> km
      km' = KM.map normalizeSchemaForStructuredOutput km1
   in Object $ case KM.lookup "type" km' of
        Just (String "object") -> KM.insert "additionalProperties" (Bool False) km'
        Just (String "integer") -> foldBoundsIntoDescription km'
        _ -> km'
normalizeSchemaForStructuredOutput (Array vs) = Array (fmap normalizeSchemaForStructuredOutput vs)
normalizeSchemaForStructuredOutput v = v

{- | Fold @"minimum"@\/@"maximum"@ into @"description"@ on a schema node,
then drop the two keywords. Appends to an existing description rather than
replacing it.
-}
foldBoundsIntoDescription :: KM.KeyMap Value -> KM.KeyMap Value
foldBoundsIntoDescription km =
  case (KM.lookup "minimum" km, KM.lookup "maximum" km) of
    (Nothing, Nothing) -> km
    (mMin, mMax) ->
      let sentence = boundsSentence mMin mMax
          existingDescription = case KM.lookup "description" km of
            Just (String d) | not (T.null d) -> Just d
            _ -> Nothing
          newDescription = maybe sentence (\d -> d <> " " <> sentence) existingDescription
       in KM.insert "description" (String newDescription) (KM.delete "minimum" (KM.delete "maximum" km))

-- | Render a minimum\/maximum pair as a sentence describing the bound(s).
boundsSentence :: Maybe Value -> Maybe Value -> Text
boundsSentence (Just mn) (Just mx) = "Must be between " <> renderNum mn <> " and " <> renderNum mx <> " (inclusive)."
boundsSentence (Just mn) Nothing = "Must be at least " <> renderNum mn <> "."
boundsSentence Nothing (Just mx) = "Must be at most " <> renderNum mx <> "."
boundsSentence Nothing Nothing = ""

-- | Render a numeric schema bound (assumed integral) without a trailing @.0@.
renderNum :: Value -> Text
renderNum (Number n) = T.pack (show (round n :: Integer))
renderNum v = T.pack (show v)

{- | Inline all @$ref@ pointers in a schema against its @$defs@ section,
producing a flat schema with no @$ref@ or @$defs@.

This is no longer needed to work around unresolved @$ref@ on the wire:
Anthropic's structured outputs support @$ref@\/@$defs@ (external URL refs
excepted), and Ollama's server resolves @$ref@ too — confirmed directly
against a live Ollama 0.30.6, on both the response @format@ field and a
tool's parameter schema, using a deliberately unnatural sub-schema so a
small model could not have guessed the field name any other way.

It survives at both of "Control.Monad.Prompt.Ollama"'s call sites for a
different reason: @ollama-haskell@'s own typed schema representations
('Data.Ollama.Common.SchemaBuilder.Schema', used for the response @format@
field, and 'Data.Ollama.Common.Types.FunctionParameters', used for a tool's
parameters) have no field for @$ref@ or @$defs@ at all. 'schemaWithDefs'
always @$ref@s its own root, so handing either conversion an un-inlined
schema collapses it to an unconstrained @json@ format or an empty parameter
object, for /every/ record type, not just ones with nested records —
confirmed directly in @cabal repl@ against this module's own functions. The
Anthropic call site, by contrast, hands a raw JSON 'Value' straight to the
wire and no longer calls this function.
-}
inlineSchema :: Value -> Value
inlineSchema root = go 20 startVal
  where
    defs = case root of
      Object km -> case KM.lookup "$defs" km of
        Just (Object d) -> d
        _ -> KM.empty
      _ -> KM.empty

    startVal = case root of
      Object km -> case KM.lookup "$ref" km of
        Just (String ref) -> resolve 19 ref
        _ -> root
      _ -> root

    resolve n ref =
      let name = T.replace "#/$defs/" "" ref
       in case KM.lookup (Key.fromText name) defs of
            Just v -> go n v
            Nothing -> Object KM.empty

    go :: Int -> Value -> Value
    go 0 v = v
    go n (Object o) = case KM.lookup "$ref" o of
      Just (String ref) -> go (n - 1) (resolve (n - 1) ref)
      _ -> Object (KM.map (go (n - 1)) (KM.delete "$defs" o))
    go n (Array vs) = Array (fmap (go (n - 1)) vs)
    go _ v = v
