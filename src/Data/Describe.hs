{- | The 'Describe' typeclass: attach a human-readable description, invariant
properties, and example values to a type, so that an LLM backend can
understand what it should produce.
-}
module Data.Describe (module Data.Describe) where

-- base

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy)
import Data.Void (Void)

-- containers
import Data.Set (Set)
import Data.Set qualified as S

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (ToJSON, Value (..), encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM

-- text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- openapi3
import Data.OpenApi (ToSchema, toSchema)

-- universe-base
import Data.Universe.Class (Universe, universe)

{- | Attach a description, invariant properties, and examples to a type.

Minimal complete definition: 'describeType'.

The associated type 'Property' enumerates the invariants that the LLM
output should satisfy.  If your type has no invariants to communicate,
leave it as the default @()@.

Use 'description' to render the full prompt fragment for a type.
-}
class (Universe (Property a), ToJSON a) => Describe a where
  {- | The type of invariant properties for @a@.  Must have a 'Universe'
    instance.  Defaults to 'Void' (no properties).
  -}
  type Property a :: Type

  type Property a = Void

  {- | A one- or two-sentence description of what the type represents.
    Use @$(deriveDescribeType ''MyType)@ from "Control.Monad.Prompt.TH" to
    derive this from the Haddock comment on the type.
  -}
  describeType :: Proxy a -> Text

  {- | Returns 'True' if the property holds for the value, 'False' if it is
    violated.  Used for validation after parsing.  Defaults to 'True'
    (all values pass all properties).
  -}
  propertyHolds :: a -> Property a -> Bool
  propertyHolds _ _ = True

  {- | Human-readable description of a property, used in the prompt.
    May be 'Nothing' if you don't wish to describe it.
  -}
  describeProperties :: Proxy a -> Property a -> Maybe Text
  describeProperties _ _ = Nothing

  {- | Representative example values.  Included in the prompt when non-empty.
    Defaults to the empty set.
  -}
  examples :: Proxy a -> Set a
  examples _ = S.empty

{- | Render the full prompt fragment for a type: its description, any
invariant properties, and example values (if any).

This is what gets sent to the LLM alongside the JSON schema.
Field descriptions are annotated with their JSON types from the OpenAPI
schema (e.g. @- name (string): The user's full name.@).
-}
description :: (Describe a, ToSchema a) => Proxy a -> Text
description p =
  T.unlines $
    [ ""
    , "Produce a value of the following type:"
    , annotateFieldTypes (toJSON (toSchema p)) (describeType p)
    ]
      <> propSection
      <> examplesSection
  where
    propDescs = mapMaybe (describeProperties p) universe
    propSection
      | null propDescs = []
      | otherwise =
          "The following invariants MUST hold in your response:"
            : fmap ("- " <>) propDescs
    examplesSection
      | S.null (examples p) = []
      | otherwise =
          "Example valid JSON responses:"
            : (toStrict . decodeUtf8 . encode <$> S.toList (examples p))

{- | For each @"- fieldName: doc"@ line in the type description, look up
@fieldName@ in the OpenAPI schema's @properties@ map and insert the JSON
type in parentheses: @"- fieldName (string): doc"@.
Lines that don't match the pattern or lack a known type are left unchanged.
-}
annotateFieldTypes :: Value -> Text -> Text
annotateFieldTypes schema = T.unlines . fmap annotateLine . T.lines
  where
    -- Extract {"fieldName": {"type": "..."}} from the schema
    schemaProps :: KM.KeyMap Value
    schemaProps = case schema of
      Object km -> case KM.lookup "properties" km of
        Just (Object props) -> props
        _ -> KM.empty
      _ -> KM.empty

    fieldType :: Text -> Text
    fieldType fieldName = case KM.lookup (Key.fromText fieldName) schemaProps of
      Just (Object fp) -> case KM.lookup "type" fp of
        Just (String t) -> t
        _ -> typeFromRef fp
      _ -> ""

    -- For $ref fields (nested objects), there's no "type" key but we can say "object"
    typeFromRef :: KM.KeyMap Value -> Text
    typeFromRef fp
      | KM.member "$ref" fp = "object"
      | otherwise = ""

    annotateLine :: Text -> Text
    annotateLine line = case T.stripPrefix "- " line of
      Nothing -> line
      Just rest ->
        let (fieldName, afterColon) = T.breakOn ":" rest
            trimmed = T.strip fieldName
            t = fieldType trimmed
         in if T.null t || T.null afterColon
              then line
              else "- " <> trimmed <> " (" <> t <> ")" <> afterColon
