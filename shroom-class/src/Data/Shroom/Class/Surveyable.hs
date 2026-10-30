{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The 'Surveyable' class: the properties and example values of a type, with
the 'Surveyable' instances for the base types, the containers and
'Generically', the structural default of the properties and the helpers that
lift an element's properties into a container.

Also here is 'description', which renders a type for the model and needs both
'Describable' and 'Surveyable'.

"Data.Shroom.Class" re-exports everything here. The instances live with the
class, so that none of them is an orphan.
-}
module Data.Shroom.Class.Surveyable where

-- base
import Data.Int (Int64)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.Typeable (typeRep)
import Data.Void (Void, absurd)
import GHC.Generics (Generically (..))
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

-- shroom-class
import Data.Shroom.Class.Describable
import Data.Shroom.Class.PropertyCheck (PropertyCheck (..), propertyLines)

-- * Surveyable

{- | Attach properties and example values to a type.

This is the main user-filled class. A bare @instance Surveyable X@ needs no
method at all: the type's properties are those of its fields, see below.

Superclass: 'Describable'.

== The default: the properties of the fields

'Property' defaults to @'Structural' X@, and 'propertyHolds' and
'describeProperties' default to 'structuralHolds' and 'describeStructural'. So
in

@
data Booking = Booking { bookingGuest :: Guest, bookingNights :: Int }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

instance Surveyable Booking
@

every property of @Guest@ is a property of @Booking@: it is described in the
prompt (as @Field bookingGuest: …@) and checked on the parsed value. The
default needs the @generics-sop@ classes @SOP.Generic@ and @SOP.HasDatatypeInfo@
for the type, both derivable (@deriving anyclass@) from "GHC.Generics"'s
@Generic@, and a @Surveyable@ instance for every field type; 'Text', the number
types, 'Bool', 'Char', 'UTCTime' and 'Day' have them here.

== Opting out: @type Property X = Void@

A type that wants no properties — a base type, a type whose fields are not
'Surveyable', or one that simply does not want the @generics-sop@ instances —
writes

@
instance Surveyable X where
  type Property X = Void
@

and nothing else: 'propertyHolds' and 'describeProperties' are then filled in
(vacuously) as well.

== A custom 'Property' replaces the default

Naming a 'Property' type of your own, say @type Property X = XProperty@, replaces
the default entirely: the properties of the fields are neither described nor
checked any more. (A @Slot@ with a @Talk@ field and a property of its own
loses the @Talk@'s properties this way.) A custom 'Property' also needs
__both__ 'propertyHolds' and 'describeProperties' written out; leaving one out
is a compile error (@No instance for ‘PropertyDefault XProperty X’ arising from a
use of ‘Data.Shroom.Class.Surveyable.$dmdescribeProperties’@, see 'PropertyDefault'), not a
silent default of 'True' or 'Nothing'.

To keep the properties of the fields next to your own, combine the two with
'Either' and hand the structural half to 'structuralHolds' and
'describeStructural':

@
instance Surveyable X where
  type Property X = Either XProperty (Structural X)
  propertyHolds x (Left p) = … -- the type's own properties
  propertyHolds x (Right p) = structuralHolds x p
  describeProperties px (Left p) = …
  describeProperties px (Right p) = describeStructural px p
@

All four equations are needed, or the instance is an incomplete pattern match.
Writing this combination by hand is interim: a derived \"Combined\" property
source, which adds a type's own properties to its structural ones, is planned in
<https://github.com/turion/shroom/issues/18 issue #18>.

== Container fields contribute no properties

A field of type @[a]@, @'NonEmpty' a@, @'Set' a@ or @'Maybe' a@ has no properties of its
own (its 'Property' is 'Void'), whatever @a@ has. So under a bare instance the
properties of the elements of a @[Speaker]@ or of a @Maybe Name@ field stay
unchecked, and not even told to the model, until container lifting (<https://github.com/turion/shroom/issues/18 issue #18>)
is derivable. For now lift them by hand with 'everyHolds' and 'describeEvery' in
a custom 'Property' combined with @'Structural' X@, as the @Speakers@ example under 'everyHolds' does.

== A recursive type with a bare instance hangs

A type that is directly or mutually recursive must not use the structural
default. \"Directly recursive\" means a field of its own type that is not under a
list, 'NonEmpty' or 'Maybe', like the @Neg Expr@ in @data Expr = Lit Int | Neg
Expr@; \"mutually recursive\" means the same through another type's bare
instance. Enumerating the structural 'Property' (its 'Universe') of such a type
never ends, and 'description' and shroom's retry loop enumerate
it, so with a bare @instance Surveyable Expr@ they run forever, in @shroom@
before any request is sent and on every backend. (Under the former @Void@
default this worked.) Opt out on the recursive type with
@type Property Expr = Void@, as above, or give it a custom 'Property' that does
not contain @'Structural' Expr@.

== A field of a type you do not own

A bare instance fails with an @All2 Surveyable@ error if one of the fields has
no 'Describable' and 'Surveyable' instance, as happens for a field of a
third-party type. Either write the opt-out above on the enclosing type, or give
the field type — or a @newtype@ around it, to avoid an orphan instance — its own
instances.
-}
class (Describable a, Universe (Property a)) => Surveyable a where
  {- | The type of properties for @a@.  Must have a 'Universe'
    instance.  Defaults to @'Structural' a@, the properties of @a@'s fields;
    write @type Property a = Void@ for no properties at all.
  -}
  type Property a :: Type

  type Property a = Structural a

  {- | Check whether the property holds.

   Returns 'True' if the property holds for the value, 'False' if it is
    violated.  Used for validation after parsing.

    Defaults to checking the 'Structural' properties of the fields if 'Property'
    is its default, and to nothing at all if it is 'Void'. For any other
    'Property' there is no default, and the method must be written.

    Hint: If you have an "informal" property (like "is an english name")
    that can't be easily checked with code,
    you can still include it in 'describeProperties' and return 'True' here.
  -}
  propertyHolds :: a -> Property a -> Bool
  default propertyHolds :: (PropertyDefault (Property a) a) => a -> Property a -> Bool
  propertyHolds = defaultHolds

  {- | Human-readable description of a property, used in the prompt.
    May be 'Nothing' if you don't wish to describe it.

    Defaults like 'propertyHolds': for the 'Structural' properties of the fields
    if 'Property' is its default, for no property at all if it is 'Void', and
    otherwise the method must be written.
  -}
  describeProperties :: Proxy a -> Property a -> Maybe Text
  default describeProperties :: (PropertyDefault (Property a) a) => Proxy a -> Property a -> Maybe Text
  describeProperties = defaultDescribe

  {- | Representative example values.  Included in the prompt when non-empty.
    Defaults to the empty set.
  -}
  examples :: Proxy a -> Set a
  examples _ = S.empty

{- | Every property of @a@ as a 'PropertyCheck': one entry per element of
'universe' of 'Property', in its order, with the description
'describeProperties' gives and the check 'propertyHolds' makes.
-}
typeProperties :: forall a. (Surveyable a) => [PropertyCheck a]
typeProperties = [PropertyCheck (describeProperties (Proxy @a) p) (`propertyHolds` p) | p <- universe @(Property a)]

-- * Base types

-- Instances for common base types, so that a type with a field of one of them
-- can derive its own instances. Each has no properties: 'Property' is 'Void',
-- written out explicitly.
--
-- They live in the module of their class, not a sibling: there they are not orphans,
-- and modules importing only "Data.Shroom.Class" see them.

-- ** Scalars

instance Surveyable Text where
  type Property Text = Void

instance Surveyable Int where
  type Property Int = Void

instance Surveyable Integer where
  type Property Integer = Void

instance Surveyable Int64 where
  type Property Int64 = Void

instance Surveyable Word where
  type Property Word = Void

instance Surveyable Natural where
  type Property Natural = Void

instance Surveyable Double where
  type Property Double = Void

instance Surveyable Float where
  type Property Float = Void

instance Surveyable Bool where
  type Property Bool = Void

instance Surveyable Char where
  type Property Char = Void

instance Surveyable UTCTime where
  type Property UTCTime = Void

instance Surveyable Day where
  type Property Day = Void

-- ** Containers

instance (Describable a) => Surveyable [a] where
  type Property [a] = Void

instance (Describable a) => Surveyable (NonEmpty a) where
  type Property (NonEmpty a) = Void

instance (Describable a) => Surveyable (Set a) where
  type Property (Set a) = Void

instance (Describable a) => Surveyable (Maybe a) where
  type Property (Maybe a) = Void

-- ** Generically

-- | Forwards to the wrapped type, see the 'Describable' instance.
instance (Surveyable a, ToJSON (Generically a)) => Surveyable (Generically a) where
  type Property (Generically a) = Property a
  propertyHolds (Generically x) = propertyHolds x
  describeProperties _ = describeProperties (Proxy @a)
  examples _ = S.mapMonotonic Generically (examples (Proxy @a))

-- * Property machinery

-- ** Lifting element properties into containers

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

-- ** Structural properties

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

A bare @instance Surveyable Person@ now does exactly this by default, so the
explicit form above is only needed when spelling it out, for example as the
@Right@ half of an 'Either' (see 'Surveyable').

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

-- ** Defaults of 'Surveyable''s methods

{- | What 'propertyHolds' and 'describeProperties' do when an instance does not
write them, chosen by the instance's 'Property' type @p@: nothing for 'Void',
the structural properties of the fields for @'Structural' a@.

This class is an implementation detail of 'Surveyable''s method defaults, and is
exported only because its name shows up in a compile error. __If GHC complains
that there is no instance of @PropertyDefault p a@, the instance of
'Surveyable' for @a@ names a custom 'Property' @p@ and leaves 'propertyHolds' or
'describeProperties' out: a custom 'Property' must define both
'propertyHolds' and 'describeProperties'.__ Do not write instances of this
class.
-}
class PropertyDefault p a where
  -- | The default 'propertyHolds'.
  defaultHolds :: a -> p -> Bool

  -- | The default 'describeProperties'.
  defaultDescribe :: Proxy a -> p -> Maybe Text

-- | No properties: nothing to check, nothing to describe.
instance PropertyDefault Void a where
  defaultHolds _ = absurd
  defaultDescribe _ = absurd

-- | The properties of the fields, see 'Structural'.
instance (Generic a, HasDatatypeInfo a, All2 Surveyable (Code a)) => PropertyDefault (Structural a) a where
  defaultHolds = structuralHolds
  defaultDescribe = describeStructural

-- * Rendering a type for the model

{- | Render the full prompt fragment for a type: its description, any
properties, and example values (if any).

This is what gets sent to the LLM alongside the JSON schema.
Field descriptions are annotated with their JSON types from the OpenAPI
schema (e.g. @- name (string): The user's full name.@).

This is what @runPrompt@ (from the @shroom@ package) sends as the prompt text
for a @RequestPrompt@.

After the examples comes a section of the types the requested type refers to,
through 'referencedTypes': each such type once (told apart by its 'TypeRep',
so @Choice Fuel@ and @Choice Phase@ are two types), breadth first from the
requested type and in the order 'referencedTypes' lists them, never the requested
type itself, and also for recursive types. Each entry is the type's name and its
'describeType', nothing more. The section is absent when nothing is reachable, so
a type without references renders as before. Types with 'hasOwnSection' 'False'
(the base types and the containers) get no entry, and a hand-written instance
that does not write 'referencedTypes' contributes none.
-}
description :: forall a. (Surveyable a, ToSchema a) => Proxy a -> Text
description p =
  T.unlines $
    [ ""
    , "Produce a value of the following type:"
    , annotateFieldTypes (toJSON (toSchema p)) (describeType p)
    ]
      <> propSection
      <> examplesSection
      <> reachableSection
  where
    reachable = reachableTypes p
    reachableSection
      | null reachable = []
      | otherwise =
          "The types above refer to the following types:"
            : concatMap (\(Described q) -> ["", T.pack (show (typeRep q)) <> ":", describeType q]) reachable
    propLines = propertyLines (typeProperties @a)
    propSection
      | null propLines = []
      | otherwise = "The following properties MUST hold in your response:" : propLines
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
