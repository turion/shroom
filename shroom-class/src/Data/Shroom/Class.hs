{-# LANGUAGE UndecidableInstances #-}

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
@openapi3@, @universe-base@, @generics-sop@ and @time@, and nothing under @Data.Shroom.@ may import
@Control.Monad.Prompt@ or any of its submodules. That is deliberate, not
incidental — a later arc (shroom-shikumi, "C3") needs exactly this pure half,
with no program layer attached, to feed shikumi's own instruction and
validation machinery. Keep it that way: an import of @Control.Monad.Prompt@
here would put the program layer back in C3's way.
-}
module Data.Shroom.Class (module Data.Shroom.Class) where

-- base

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (mapMaybe)
import Data.Proxy (Proxy (..))
import Data.Void (Void)
import Numeric.Natural (Natural)

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

-- time
import Data.Time (Day, UTCTime)

-- universe-base
import Data.Universe.Class (Universe, universe)

-- generics-sop
import Generics.SOP (All, All2, Code, ConstructorInfo (..), FieldInfo (..), Generic, HasDatatypeInfo (..), I (..), K (..), NP (..), NS (..), SList (..), constructorInfo, constructorName, from, hcollapse, hmap, lengthSList, sList, unSOP)

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

-- * Base types

-- Instances for common base types, so that a type with a field of one of them
-- can derive its own instances. Each describes the type in a short plain
-- sentence and has no properties: 'Property' is 'Void', written out explicitly.
--
-- They live in this module itself, not a sibling: here they are not orphans,
-- and modules importing only "Data.Shroom.Class" see them.

instance Describable Text where
  describeType _ = "A piece of text."

instance Surveyable Text where
  type Property Text = Void

instance Describable Int where
  describeType _ = "A whole number."

instance Surveyable Int where
  type Property Int = Void

instance Describable Integer where
  describeType _ = "A whole number."

instance Surveyable Integer where
  type Property Integer = Void

instance Describable Natural where
  describeType _ = "A non-negative whole number."

instance Surveyable Natural where
  type Property Natural = Void

instance Describable Double where
  describeType _ = "A number, possibly with a fractional part."

instance Surveyable Double where
  type Property Double = Void

instance Describable Float where
  describeType _ = "A number, possibly with a fractional part."

instance Surveyable Float where
  type Property Float = Void

instance Describable Bool where
  describeType _ = "A truth value: true or false."

instance Surveyable Bool where
  type Property Bool = Void

instance Describable Char where
  describeType _ = "A single character."

instance Surveyable Char where
  type Property Char = Void

instance Describable UTCTime where
  describeType _ = "A point in time, in UTC."

instance Surveyable UTCTime where
  type Property UTCTime = Void

instance Describable Day where
  describeType _ = "A calendar date."

instance Surveyable Day where
  type Property Day = Void

{- | A list, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'everyHolds' and 'describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable [a] where
  describeType _ = "A list of values of the following type: " <> describeType (Proxy @a)

instance (Describable a) => Surveyable [a] where
  type Property [a] = Void

{- | A non-empty list, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'everyHolds' and 'describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable (NonEmpty a) where
  describeType _ = "A non-empty list of values of the following type: " <> describeType (Proxy @a)

instance (Describable a) => Surveyable (NonEmpty a) where
  type Property (NonEmpty a) = Void

{- | An optional value, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'everyHolds' and 'describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable (Maybe a) where
  describeType _ = "A value that may be absent (null), and otherwise of the following type: " <> describeType (Proxy @a)

instance (Describable a) => Surveyable (Maybe a) where
  type Property (Maybe a) = Void

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

-- * Structural properties

{- | The properties of one field type @x@, wrapped in a newtype so that a
sum over fields ('NS') can carry them. A data type rather than a type family
application, which a partially applied 'Property' could not be.
-}
newtype PropertyOf x = PropertyOf (Property x)

deriving instance (Eq (Property x)) => Eq (PropertyOf x)

deriving instance (Show (Property x)) => Show (PropertyOf x)

{- | The properties of a type that follow from its structure: a property of
one field of one constructor, which is a property of the field's own type.

A 'Structural' value picks a constructor of @a@, then one of that
constructor's fields, then one of the field type's own 'Property's. A type
whose fields are all 'Surveyable' can use it as its own 'Property' type
without writing out any property:

@
instance Surveyable Person where
  type Property Person = Structural Person
  propertyHolds = structuralHolds
  describeProperties = describeStructural
@

(For this the type needs the @generics-sop@ classes 'Generic' and
'HasDatatypeInfo', both derivable from "GHC.Generics"'s own @Generic@.)

It is a newtype, not a type family, so that naming it as a 'Property' needs
no @UndecidableInstances@ in the user's module.

Its 'Universe' lists every field property of every constructor, in
constructor and then field order. A constructor without fields, and a field
whose own 'Property' is empty, contribute nothing.
-}
newtype Structural a = Structural (NS (NS PropertyOf) (Code a))

deriving instance (Eq (NS (NS PropertyOf) (Code a))) => Eq (Structural a)

deriving instance (Show (NS (NS PropertyOf) (Code a))) => Show (Structural a)

instance (All2 Surveyable (Code a)) => Universe (Structural a) where
  universe = Structural <$> universeConstructors

universeConstructors :: forall xss. (All2 Surveyable xss) => [NS (NS PropertyOf) xss]
universeConstructors = case sList :: SList xss of
  SNil -> []
  SCons -> (Z <$> universeFields) ++ (S <$> universeConstructors)

universeFields :: forall xs. (All Surveyable xs) => [NS PropertyOf xs]
universeFields = case sList :: SList xs of
  SNil -> []
  SCons -> (Z . PropertyOf <$> universe) ++ (S <$> universeFields)

{- | Check a structural property of a value.

A property of constructor @c@, field @f@ holds vacuously (as in 'everyHolds')
for a value built with a different constructor than @c@. For a value built
with @c@, it holds exactly when the field's own 'propertyHolds' does.
-}
structuralHolds :: (Generic a, All2 Surveyable (Code a)) => a -> Structural a -> Bool
structuralHolds x (Structural p) = holdsConstructor (unSOP (from x)) p

holdsConstructor :: (All2 Surveyable xss) => NS (NP I) xss -> NS (NS PropertyOf) xss -> Bool
holdsConstructor (Z vs) (Z ps) = holdsField vs ps
holdsConstructor (S vs) (S ps) = holdsConstructor vs ps
holdsConstructor _ _ = True

holdsField :: (All Surveyable xs) => NP I xs -> NS PropertyOf xs -> Bool
holdsField (I v :* _) (Z (PropertyOf p)) = propertyHolds v p
holdsField (_ :* vs) (S ps) = holdsField vs ps
holdsField Nil p = case p of {}

{- | Describe a structural property: the field's own 'describeProperties',
prefixed with where it lives.

* A record field is named @Field lastName: …@, a positional one by its
  position and constructor, @Field 2 of Pair: …@.
* For a type with several constructors, the constructor is named as well:
  @Constructor FruitApple, field kernels: …@ or
  @Constructor FruitBanana, field 1: …@.

'Nothing' exactly when the field's own description of the property is.
-}
describeStructural :: forall a. (HasDatatypeInfo a, All2 Surveyable (Code a)) => Proxy a -> Structural a -> Maybe Text
describeStructural _ (Structural p) = describeConstructor multi (constructorInfo (datatypeInfo (Proxy @a))) p
  where
    multi = lengthSList (Proxy @(Code a)) > 1

describeConstructor :: (All2 Surveyable xss) => Bool -> NP ConstructorInfo xss -> NS (NS PropertyOf) xss -> Maybe Text
describeConstructor multi (c :* _) (Z ps) = describeField prefix 0 ps
  where
    conName = T.pack (constructorName c)
    prefix i = case fieldNameAt i c of
      Just name
        | multi -> "Constructor " <> conName <> ", field " <> name
        | otherwise -> "Field " <> name
      Nothing
        | multi -> "Constructor " <> conName <> ", field " <> T.pack (show (i + 1))
        | otherwise -> "Field " <> T.pack (show (i + 1)) <> " of " <> conName
describeConstructor multi (_ :* cs) (S ps) = describeConstructor multi cs ps
describeConstructor _ Nil p = case p of {}

describeField :: (All Surveyable xs) => (Int -> Text) -> Int -> NS PropertyOf xs -> Maybe Text
describeField prefix i (Z p) = (\d -> prefix i <> ": " <> d) <$> describeOne p
describeField prefix i (S ps) = describeField prefix (i + 1) ps

describeOne :: forall x. (Surveyable x) => PropertyOf x -> Maybe Text
describeOne (PropertyOf p) = describeProperties (Proxy @x) p

-- | The name of the record field at a position, 'Nothing' for a positional constructor.
fieldNameAt :: Int -> ConstructorInfo xs -> Maybe Text
fieldNameAt i (Record _ fields) = case drop i (hcollapse (hmap (\(FieldInfo name) -> K (T.pack name)) fields)) of
  name : _ -> Just name
  [] -> Nothing
fieldNameAt _ _ = Nothing

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
