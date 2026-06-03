{- | The 'Describable', 'Surveyable', and 'Promptable' typeclasses: attach a
human-readable description, property properties, example values, and a prompt
rendering strategy to a type, so that an LLM backend can understand what it
should produce.

== Typeclass hierarchy

@
Describable          — 'describeType': one sentence from Haddock (auto-derived)
    └── Surveyable   — 'Property', 'propertyHolds', 'describeProperties', 'examples' (user-filled)
            └── Promptable (+ ToSchema)  — 'promptDescription': how to render the full prompt
@

Typical usage:

@
-- | A user with a name and an email address.
data User = User { userName :: Text, userEmail :: Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

\$(deriveDescribable ''User)   -- generates instance Describable User from Haddock

instance Surveyable User where
  type Property User = UserProperty
  propertyHolds u UserEmailNotEmpty = not (T.null (userEmail u))
  describeProperties _ UserEmailNotEmpty = Just "The email must not be empty."

instance Promptable User  -- uses default: description-based prompt rendering
@
-}
module Data.Shroom.Class (module Data.Shroom.Class) where

-- base

import Data.Kind (Type)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Void (Void)

-- containers
import Data.Set (Set)
import Data.Set qualified as S

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON, Value (..), encode, toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM

-- text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- openapi3
import Data.OpenApi (ToSchema, toSchema)

-- universe-base
import Data.Universe.Class (Universe, universe)

-- * Describable

{- | Attach a one-sentence description to a type.

Minimal complete definition: 'describeType'.

Use @$(deriveDescribable ''MyType)@ from "Control.Monad.Prompt.TH" to derive
this automatically from all Haddock comments on the type declaration.
-}
class (ToJSON a) => Describable a where
  {- | A one- or two-sentence description of what the type represents,
    and what the constructors and fields mean.
  -}
  describeType :: Proxy a -> Text

-- * Surveyable

{- | Attach property properties and example values to a type.

This is the main user-filled class. Override the methods you need;
all have sensible defaults (no properties, no examples).

Superclass: 'Describable'.
-}
class (Describable a, Universe (Property a)) => Surveyable a where
  {- | The type of property properties for @a@.  Must have a 'Universe'
    instance.  Defaults to 'Void' (no properties).
  -}
  type Property a :: Type

  type Property a = Void

  {- | Check whether the property holds.

   Returns 'True' if the property holds for the value, 'False' if it is
    violated.  Used for validation after parsing.

    Defaults to 'True' (all values pass all properties).

    Hint: If you have an "informal" property (like "is an english name")
    that can't be easily checked with code,
    you can still include it in 'describeProperties' and return 'True' here.
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

-- * Promptable

{- | Determines how to render the full prompt description for a type.

The default 'promptDescription' calls 'description', which renders:

* 'describeType' — the one-sentence description
* properties from 'describeProperties'
* example values from 'examples'
* field types annotated from the OpenAPI schema

Override 'promptDescription' for fully custom prompt text.

Superclass: 'Surveyable' and 'ToSchema'.
-}
class (Surveyable a, ToSchema a) => Promptable a where
  {- | Render the full prompt fragment for a type, sent to the LLM alongside
    the JSON schema.
  -}
  promptDescription :: Proxy a -> Text

{- | Helper newtype for using the default description-based prompt rendering strategy.

Use with `DerivingVia`:

@
data MyType = MyType { ... }

-- TH call: Must go first
\$(deriveDescribable ''MyType)

instance Surveyable User where

-- Must come after TH call, therefore standalone deriving via
deriving via DescriptionPrompt MyType instance Promptable MyType
@
-}
newtype DescriptionPrompt a = DescriptionPrompt {getDescriptionPrompt :: a}
  deriving stock (Eq, Show)
  deriving newtype (ToJSON, FromJSON, ToSchema)

instance (Describable a) => Describable (DescriptionPrompt a) where
  describeType _ = describeType (Proxy @a)

instance (Surveyable a) => Surveyable (DescriptionPrompt a) where
  type Property (DescriptionPrompt a) = Property a
  describeProperties _ = describeProperties (Proxy @a)
  propertyHolds (DescriptionPrompt x) = propertyHolds x

instance (Surveyable a, ToSchema a) => Promptable (DescriptionPrompt a) where
  promptDescription _ = description (Proxy @a)

-- * description helper functions

{- | Render the full prompt fragment for a type: its description, any
property properties, and example values (if any).

This is what gets sent to the LLM alongside the JSON schema.
Field descriptions are annotated with their JSON types from the OpenAPI
schema (e.g. @- name (string): The user's full name.@).

This is also the default implementation of 'promptDescription'.
-}
description :: (Surveyable a, ToSchema a) => Proxy a -> Text
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
          "The following properties MUST hold in your response:"
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
