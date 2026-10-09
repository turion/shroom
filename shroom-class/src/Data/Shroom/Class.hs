{- | The 'Describable' and 'Surveyable' typeclasses: attach a human-readable
description, properties, and example values to a type, so that an
LLM backend — or anything else that renders a type for a reader, human or
model — can understand what it should produce.

== Typeclass hierarchy

@
Describable          — 'describeType': one sentence from Haddock (auto-derived)
    └── Surveyable   — 'Property', 'propertyHolds', 'describeProperties', 'examples' (user-filled)
@

@Promptable@, which builds on 'Surveyable' to render a full prompt, lives on
the far side of the pure\/effectful boundary — in the @shroom@ package, as
@Control.Monad.Prompt.Promptable@ — see below.

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

instance Promptable User  -- from Control.Monad.Prompt.Promptable; uses the
                          -- default description-based prompt rendering
@

== Pure\/effectful boundary

This module is pure, and the package boundary coincides with that line: it
lives in @shroom-class@, which has no program layer, while everything effectful
lives in @shroom@. It depends only on @aeson@, @text@, @containers@,
@openapi3@ and @universe-base@, and nothing under @Data.Shroom.@ may import
@Control.Monad.Prompt@ or any of its submodules. That is deliberate, not
incidental — a later arc (shroom-shikumi, "C3") needs exactly this pure half,
with no program layer attached, to feed shikumi's own instruction and
validation machinery. Keep it that way: an import of @Control.Monad.Prompt@
here would put the program layer back in C3's way.
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

{- | Attach properties and example values to a type.

This is the main user-filled class. Override the methods you need;
all have sensible defaults (no properties, no examples).

Superclass: 'Describable'.
-}
class (Describable a, Universe (Property a)) => Surveyable a where
  {- | The type of properties for @a@.  Must have a 'Universe'
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

-- * Lifting element properties into containers

{- | Check that one of an element type's properties holds for every element
of a container. Use it to lift the element's properties into the container's
own 'Property' type, as one side of an 'Either':

@
instance Surveyable Speakers where
  type Property Speakers = Either SpeakersProperty SpeakerProperty
  propertyHolds s (Left SpeakersBetween3And10) = ...
  propertyHolds s (Right p) = everyHolds (speakers s) p
  describeProperties _ (Left SpeakersBetween3And10) = ...
  describeProperties _ (Right p) = describeEvery \"Every speaker\" (Proxy \@Speaker) p
@

Name the element's concrete property type (here @SpeakerProperty@) rather than
writing @Property Speaker@ in the instance head: GHC rejects the latter without
@UndecidableInstances@, as a type family application is no smaller than the
instance's own left-hand side.

@runPrompt@ (from the @shroom@ package) only checks the properties of the
top-level value, and 'description' only lists those, so without this lift a
property of the elements is neither told to the model nor checked.
This is a hand-written helper; a derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.

Vacuously 'True' for an empty container.
-}
everyHolds :: (Foldable f, Surveyable a) => f a -> Property a -> Bool
everyHolds xs p = all (`propertyHolds` p) xs

{- | Describe an element's property as a property of every element of a
container, to be used from 'describeProperties' alongside 'everyHolds'.
The label is joined to the element's own description with a colon, e.g.
@describeEvery \"Every speaker\"@ turns \"The speaker's last name must not be
empty.\" into \"Every speaker: The speaker's last name must not be empty.\".

Returns 'Nothing' when the element has no description for the property.
See 'everyHolds' for the pattern, and
<https://github.com/turion/shroom/issues/18 issue #18> for the derivable version.
-}
describeEvery :: (Surveyable a) => Text -> Proxy a -> Property a -> Maybe Text
describeEvery label p prop = (\d -> label <> ": " <> d) <$> describeProperties p prop

-- * description helper functions

{- | Render the full prompt fragment for a type: its description, any
properties, and example values (if any).

This is what gets sent to the LLM alongside the JSON schema.
Field descriptions are annotated with their JSON types from the OpenAPI
schema (e.g. @- name (string): The user's full name.@).

This is what @runPrompt@ (from the @shroom@ package) sends as the prompt text
for a @RequestPrompt@.
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
