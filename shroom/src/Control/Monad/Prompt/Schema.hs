{- | Internal schema utilities shared by 'Control.Monad.Prompt' and
'Control.Monad.Prompt.Tool'.  Not part of the public API.
-}
module Control.Monad.Prompt.Schema (encodeJson, schemaWithDefs, normalizeSchemaForStructuredOutput, inlineSchema, ToolDef (..), ToolDispatcher) where

-- base
import Data.Foldable (toList)
import Data.Maybe (catMaybes)
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
  the constraint. @openapi3@'s true\/false @"exclusiveMinimum"@\/@"exclusiveMaximum"@
  is folded with them.
* Rewrites @"oneOf"@ to @"anyOf"@, the form structured-output schemas support.
  The alternatives that @openapi3@ generates for a
  sum are told apart by their tag, so \"at least one\" accepts what \"exactly
  one\" did. A node that already has an @anyOf@ keeps it, and its @oneOf@ moves
  into an extra @allOf@ entry so that both constraints still apply.

* Keeps a string node's @"format"@ only where structured outputs support it.
  @openapi3@'s format for @UTCTime@ becomes @date-time@, which @aeson@'s
  encoding of a @UTCTime@ satisfies. Any other unsupported format moves into
  the node's @"description"@, so the model still hears it.

Whether a host enforces a supported format varies; the properties of
@Surveyable@ in "Data.Shroom.Class" check in code what the schema does not
enforce.
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
        Just (String "string") -> normalizeStringFormat km'
        _ -> km'
normalizeSchemaForStructuredOutput (Array vs) = Array (fmap normalizeSchemaForStructuredOutput vs)
normalizeSchemaForStructuredOutput v = v

{- | Fold @"minimum"@\/@"maximum"@, with a true\/false
@"exclusiveMinimum"@\/@"exclusiveMaximum"@, into @"description"@ on a schema
node, then drop those keywords. Appends to an existing description rather than
replacing it.
-}
foldBoundsIntoDescription :: KM.KeyMap Value -> KM.KeyMap Value
foldBoundsIntoDescription km =
  let bound key exclusiveKey = (,KM.lookup exclusiveKey km == Just (Bool True)) <$> KM.lookup key km
      -- Only openapi3's true/false form of the exclusive keywords is removed; a numeric one is a bound of its own.
      dropFlag key m = case KM.lookup key m of
        Just (Bool _) -> KM.delete key m
        _ -> m
      stripped = dropFlag "exclusiveMinimum" $ dropFlag "exclusiveMaximum" $ KM.delete "minimum" $ KM.delete "maximum" km
   in case (bound "minimum" "exclusiveMinimum", bound "maximum" "exclusiveMaximum") of
        (Nothing, Nothing) -> stripped
        (lower, upper) -> appendToDescription (boundsSentence lower upper) stripped

{- | Keep a string node's @"format"@ if structured outputs support it, rename
@openapi3@'s @UTCTime@ format to @date-time@, and fold any other format into
@"description"@.
-}
normalizeStringFormat :: KM.KeyMap Value -> KM.KeyMap Value
normalizeStringFormat km = case KM.lookup "format" km of
  Just (String "yyyy-mm-ddThh:MM:ssZ") -> KM.insert "format" (String "date-time") km
  Just (String format)
    | format `notElem` supportedStringFormats ->
        appendToDescription ("Format: " <> format <> ".") (KM.delete "format" km)
  _ -> km

-- | The string formats that structured-output schemas accept.
supportedStringFormats :: [Text]
supportedStringFormats = ["date-time", "date", "time", "duration", "email", "hostname", "uri", "ipv4", "ipv6", "uuid"]

-- | Append a sentence to a schema node's @"description"@, or make it the description if there is none.
appendToDescription :: Text -> KM.KeyMap Value -> KM.KeyMap Value
appendToDescription sentence km =
  let newDescription = case KM.lookup "description" km of
        Just (String d) | not (T.null d) -> d <> " " <> sentence
        _ -> sentence
   in KM.insert "description" (String newDescription) km

-- | Render a lower and an upper bound, each with whether it is exclusive, as a sentence.
boundsSentence :: Maybe (Value, Bool) -> Maybe (Value, Bool) -> Text
boundsSentence Nothing Nothing = ""
boundsSentence (Just (mn, False)) (Just (mx, False)) = "Must be between " <> renderNum mn <> " and " <> renderNum mx <> " (inclusive)."
boundsSentence lower upper = "Must be " <> T.intercalate " and " (catMaybes [lowerPhrase <$> lower, upperPhrase <$> upper]) <> "."
  where
    lowerPhrase (n, exclusive) = (if exclusive then "greater than " else "at least ") <> renderNum n
    upperPhrase (n, exclusive) = (if exclusive then "less than " else "at most ") <> renderNum n

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
