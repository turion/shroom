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

{- | Build a JSON schema 'Value' for type @a@, fully self-contained with no
@$ref@\/@$defs@ left in it, using openapi3's 'declareSchemaRef'.

Internally this first rewrites the @#\/components\/schemas\/X@ reference form
openapi3 emits by default into the @#\/$defs\/X@ form a structured-output
schema uses, via 'normalizeSchemaForStructuredOutput', then flattens every
@$ref@ against its @$defs@ via 'inlineSchema'.

The flattening step is load-bearing, not cosmetic: a live Ollama
(@0.30.6@, @llama3.2:1b@) resolves a single @$ref@ hop correctly —
confirmed directly, including on a deliberately unguessable field name —
but silently stops enforcing the schema (falling back to an unconstrained
node, observed as an empty array or invented field names rather than an
error) as soon as the wire schema asks it to follow a /second/ hop: a
@$ref@ found while resolving another @$ref@, which is exactly what any
nested-record type produces once it references another named schema (an
array of records, or a record containing a record). A flat record's own
schema is only ever one hop from the root, which is why that case worked
before this fix and a nested one did not. See this package's arc plan,
todo 20, for the direct wire evidence.
-}
schemaWithDefs :: forall a. (ToSchema a) => Proxy a -> Value
schemaWithDefs prx =
  let (defs, ref) = runDeclare (declareSchemaRef prx) mempty
      rootSchema = case ref of
        Inline s -> toJSON s
        Ref r -> Object (KM.singleton "$ref" (String ("#/$defs/" <> getReference r)))
      defsJson = toJSON defs
      normalized = case defsJson of
        Object defsKm | not (KM.null defsKm) ->
          case rootSchema of
            Object rootKm -> normalizeSchemaForStructuredOutput (Object (KM.insert "$defs" (Object defsKm) rootKm))
            _ -> normalizeSchemaForStructuredOutput rootSchema
        _ -> normalizeSchemaForStructuredOutput rootSchema
   in inlineSchema normalized

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

@$ref@\/@$defs@ is not simply unsupported on the wire — a single hop
resolves correctly even against a small local Ollama model, on both the
response @format@ field and a tool's parameter schema, confirmed directly
with a deliberately unguessable field name. What fails is a /second/ hop:
a @$ref@ found while resolving another @$ref@, which is exactly what any
nested-record type produces (an array of records, or a record containing
a record) once 'schemaWithDefs' leaves @$ref@\/@$defs@ in place. A live
Ollama (@0.30.6@) does not error on this; it silently stops enforcing the
schema at that point, observed as an empty array or invented field names.
This is why 'schemaWithDefs' now calls this function on its own result
before returning — see its Haddock and this package's arc plan, todo 20,
for the wire evidence.

Recursion is capped at 20 levels (see 'go') rather than unrolled fully: a
genuinely self-referential type has no finite fully-inlined form, so past
the cap whatever is left simply stops being descended into, rather than
looping forever.
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
