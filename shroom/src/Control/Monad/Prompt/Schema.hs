{- | Internal schema utilities shared by 'Control.Monad.Prompt' and
'Control.Monad.Prompt.Tool'.  Not part of the public API.
-}
module Control.Monad.Prompt.Schema (schemaWithDefs, fixSchemaForAnthropic, inlineSchema, ToolDef (..), ToolDispatcher) where

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
This avoids the @#\/components\/schemas\/X@ references that Anthropic rejects.
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
            Object rootKm -> fixSchemaForAnthropic (Object (KM.insert "$defs" (Object defsKm) rootKm))
            _ -> fixSchemaForAnthropic rootSchema
        _ -> fixSchemaForAnthropic rootSchema

{- | Fix a JSON schema 'Value' to satisfy Anthropic API constraints:

* Adds @"additionalProperties": false@ to every @"type": "object"@ node.
* Removes @"minimum"@ and @"maximum"@ from every @"type": "integer"@ node.
* Rewrites @$ref@ values from @#\/components\/schemas\/X@ to @#\/$defs\/X@.
-}
fixSchemaForAnthropic :: Value -> Value
fixSchemaForAnthropic (Object km) =
  let km1 = case KM.lookup "$ref" km of
        Just (String ref) -> KM.insert "$ref" (String (T.replace "#/components/schemas/" "#/$defs/" ref)) km
        _ -> km
      km' = KM.map fixSchemaForAnthropic km1
   in Object $ case KM.lookup "type" km' of
        Just (String "object") -> KM.insert "additionalProperties" (Bool False) km'
        Just (String "integer") -> KM.delete "minimum" (KM.delete "maximum" km')
        Just (String "string") -> KM.delete "format" km'
        _ -> km'
fixSchemaForAnthropic (Array vs) = Array (fmap fixSchemaForAnthropic vs)
fixSchemaForAnthropic v = v

{- | Inline all @$ref@ pointers in a schema against its @$defs@ section,
producing a flat schema with no @$ref@ or @$defs@.

Anthropic and Ollama do not resolve @$ref@ in tool/format schemas, so this
must be applied before sending tool input schemas to either API.
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
