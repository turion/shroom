{- | Internal schema utilities shared by 'Control.Monad.Prompt' and
'Control.Monad.Prompt.Tool'.  Not part of the public API.
-}
module Control.Monad.Prompt.Schema (encodeJson, schemaWithDefs, normalizeSchemaForStructuredOutput, inlineSchema, ToolDef (..), ToolDispatcher) where

-- base
import Data.Foldable (toList)
import Data.Proxy (Proxy)

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)

-- aeson
import Data.Aeson (ToJSON, Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Text (encodeToLazyText)

-- vector
import Data.Vector qualified as V

-- openapi3
import Data.OpenApi (Referenced (..), ToSchema, declareSchemaRef, getReference)
import Data.OpenApi.Declare (runDeclare)

-- | Encode a value as compact JSON text.
encodeJson :: (ToJSON v) => v -> Text
encodeJson = toStrict . encodeToLazyText

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

{- | Build the JSON schema of type @a@, self-contained with no @$ref@ or @$defs@
left in it. It takes the schema @openapi3@ derives, normalises it for
structured-output use and then inlines every reference.

A backend uses it to describe the expected result type and a tool's input to
the model.
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
  field's @"description"@ and removes the keywords, so the model still hears
  the constraint.
* Rewrites @"oneOf"@ to @"anyOf"@, the form structured-output schemas support.
  The alternatives that @openapi3@ generates for a
  sum are told apart by their tag, so \"at least one\" accepts what \"exactly
  one\" did. A node that already has an @anyOf@ keeps it, and its @oneOf@ moves
  into an extra @allOf@ entry so that both constraints still apply.

String @"format"@ is left alone. Whether a host enforces it varies; the
properties of @Surveyable@ in "Data.Shroom.Class" check in code what the schema
does not enforce.
-}
normalizeSchemaForStructuredOutput :: Value -> Value
normalizeSchemaForStructuredOutput (Object km) =
  let km1 = case KM.lookup "$ref" km of
        Just (String ref) -> KM.insert "$ref" (String (T.replace "#/components/schemas/" "#/$defs/" ref)) km
        _ -> km
      km2 = KM.map normalizeSchemaForStructuredOutput km1
      km' = case KM.lookup "oneOf" km2 of
        -- Only an array is the keyword; an object under that key is a property named @oneOf@.
        Just alternatives@(Array _) -> case KM.lookup "anyOf" km2 of
          Nothing -> KM.insert "anyOf" alternatives (KM.delete "oneOf" km2)
          Just _ ->
            let existing = case KM.lookup "allOf" km2 of
                  Just (Array vs) -> toList vs
                  _ -> []
             in KM.insert "allOf" (Array (V.fromList (existing <> [Object (KM.singleton "anyOf" alternatives)]))) (KM.delete "oneOf" km2)
        _ -> km2
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

Recursion is capped at 20 levels: a self-referential type has no finite fully
inlined form, so past the cap whatever is left is not descended into.
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
