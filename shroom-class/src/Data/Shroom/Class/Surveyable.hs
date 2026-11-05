{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The 'Surveyable' class: the properties and example values of a type, with
the 'Surveyable' instances for the base types, the containers and
'Generically', the structural properties and the helpers that
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
import Data.Typeable (Typeable, typeRep)
import Data.Void (Void, absurd)
import GHC.Generics (Generically (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Numeric.Natural (Natural)

-- containers
import Data.Set (Set)

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
import Data.Universe.Class (Finite (..), Universe (..))

-- generics-sop
import Generics.SOP (All, All2, Code, ConstructorInfo (..), FieldInfo (..), Generic, HasDatatypeInfo (..), I (..), K (..), NP (..), NS (..), SList (..), constructorInfo, constructorName, from, hcollapse, hmap, lengthSList, sList, unSOP)

-- shroom-class
import Data.Shroom.Class.Describable
import Data.Shroom.Class.PropertyCheck (PropertyCheck (..), propertyLines)

-- * Surveyable

{- | Attach properties and example values to a type.

This is the main user-filled class. Every instance states its 'Property' type:
there is __no default__, and an instance that leaves @type Property@ out does not
compile. 'propertyHolds' and 'describeProperties' are then filled in for the two
property types that need nothing written, @'Void'@ and @'Structural' X@, so an
instance of either writes only its @type Property@ (and, if it wants them,
'examples').

Superclass: 'Describable'.

== There is no default for 'Property'

An instance that does not state it, as in

@
instance Surveyable X where
  examples _ = [...]
@

is rejected with these errors (GHC 9.12.3; the second one twice, once for
'propertyHolds' and once for 'describeProperties'):

@
No instance for ‘Finite (Property X)’
  arising from the superclasses of an instance declaration
No instance for ‘StandardProperties (Property X) X’
  arising from a use of ‘Data.Shroom.Class.Surveyable.$dmpropertyHolds’
@

Neither says so, but the fix is to write @type Property X = …@ in the instance:
@Property X@ stays unreduced in both messages because the instance names no
type for it. Pick one of the common choices below; migrating an instance that
used to be bare, @type Property X = 'Structural' X@ keeps the properties of its
fields as before and @type Property X = 'Void'@ drops them. (Compare the error
that names a concrete property type, @No instance for ‘StandardProperties
XProperty X’@: that one is a custom 'Property' whose instance leaves a method
out, see below and 'StandardProperties'.)

=== @type Property X = Void@: no properties

A type that wants no properties — a base type, a type whose fields are not
'Surveyable', or one that simply does not want the @generics-sop@ instances —
writes

@
instance Surveyable X where
  type Property X = Void
@

and nothing else: 'propertyHolds' and 'describeProperties' are filled in
(vacuously) as well.

=== @type Property X = Structural X@: the properties of the fields

In

@
data Booking = Booking { bookingGuest :: Guest, bookingNights :: Int }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema, SOP.Generic, SOP.HasDatatypeInfo)

instance Surveyable Booking where
  type Property Booking = Structural Booking
@

every property of @Guest@ is a property of @Booking@: it is described in the
prompt (as @Field bookingGuest: …@) and checked on the parsed value, by
'structuralHolds' and 'describeStructural', which 'propertyHolds' and
'describeProperties' default to for this property type. The caveats:

* It needs the @generics-sop@ classes @SOP.Generic@ and @SOP.HasDatatypeInfo@
  for the type, both derivable (@deriving anyclass@) from "GHC.Generics"'s
  @Generic@, and a @Surveyable@ instance for every field type; 'Text', the number
  types, 'Bool', 'Char', 'UTCTime' and 'Day' have them here.
* A type recursive through its own structural properties never ends, see the
  recursion section below.

=== @type Property X = Either XProperty (Structural X)@: both

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

The caveat is that this is written by hand, and all four equations are needed, or
the instance is an incomplete pattern match. It has the needs and the recursion
caveat of 'Structural'. 'Either' lists the type's own properties (@Left@) before
those of the fields (@Right@), in the prompt and in the order of repair.
Writing this combination by hand is interim: a derived \"Combined\" property
source, which adds a type's own properties to its structural ones, is planned in
<https://github.com/turion/shroom/issues/18 issue #18>.

=== An enumeration of your own

A type with properties of its own and no use for those of its fields names an
enumeration, which needs @Bounded@, @Enum@, @Universe@ and @Finite@
(@universe-base@'s @Data.Universe.Class@), and writes both methods:

@
data XProperty = XNotEmpty | XIsReal
  deriving (Bounded, Enum, Universe, Finite, Eq, Ord, Show)

instance Surveyable X where
  type Property X = XProperty
  propertyHolds x XNotEmpty = …
  propertyHolds _ XIsReal = True  -- informal: told to the model, not checked
  describeProperties _ XNotEmpty = Just \"…\"
  describeProperties _ XIsReal = Just \"…\"
@

The caveat is that this __replaces__ the properties of the fields: they are
neither described nor checked any more. (A @Slot@ with a @Talk@ field and a
property of its own loses the @Talk@'s properties this way; the 'Either' form
above keeps them.) Both methods are needed: leaving one out is the error that
names @StandardProperties XProperty X@, see below.

=== Forwarding a field's properties

A type whose properties are those of one of its fields names that field's
'Property', and writes the methods by hand, handing over to the field's:

@
instance Surveyable Section where
  type Property Section = Property Title
  propertyHolds s p = propertyHolds (heading s) p
  describeProperties _ p = describeProperties (Proxy \@Title) p
@

The caveat is that @type Property Section = Property Title@ needs
@UndecidableInstances@ in your module, as the type family application is no
smaller than the left-hand side; naming @Title@'s concrete property type
instead, say @type Property Section = TitleProperty@, does not. The recursion
section below uses this form for a recursive type.

=== A 'Foldable' container: 'Every'

A container of your own that wraps a 'Foldable' has its element's properties with
one @deriving via@ line:

@
deriving via (Every \"Every entry\" Roster a) instance (Surveyable a) => Surveyable (Roster a)
@

The caveats: it needs @Surveyable a@ for the element, @DerivingVia@ and
@UndecidableInstances@ in your module, and a 'Describable' instance of your own
for the container. See 'Every' for the definition, which also tells how a
container with properties of its own besides its elements' lifts them by hand.

== A custom 'Property' needs both methods

A 'Property' type of your own, say @type Property X = XProperty@, needs
__both__ 'propertyHolds' and 'describeProperties' written out; leaving one out
is a compile error (@No instance for ‘StandardProperties XProperty X’ arising from a
use of ‘Data.Shroom.Class.Surveyable.$dmdescribeProperties’@, see
'StandardProperties'), not a silent default of 'True' or 'Nothing'. Note the
difference to the error of a missing @type Property@ above: there the
'Property' is unreduced, here it is your concrete type.

== Container fields have their elements' properties

A field of type @[a]@, @'NonEmpty' a@, @'Set' a@ or @'Maybe' a@ has the
properties of @a@: the container's 'Property' is @a@'s, told to the model under
a label (\"Every element: …\", for 'Maybe' \"If present: …\") and checked on every
element, vacuously for 'Nothing' and an empty container. So under
@'Structural' X@ the properties of the elements of a @[Speaker]@ or of a @Maybe
Name@ field are described in the prompt and checked like those of any other
field, as @Field speakers: Every element: …@. This needs @Surveyable a@, not only
'Describable'. See 'Every' for the definition, which a 'Foldable'
container of your own can use the same way, in one @deriving via@ line.

A reply that passed under the old behaviour (when the properties of container
elements were not checked) can therefore now fail a property of an element and
get a repair turn.

== A type recursive through its structural properties never ends under 'Structural'

A type that is directly or mutually recursive must not have @'Structural'@ as its
'Property': enumerating the structural 'Property' (its 'Universe') of such a type
never ends, although its 'Finite' instance claims it does, and 'description' and
shroom's retry loop enumerate it. They run forever, in @shroom@ before any request
is sent and on every backend. \"Directly recursive\" means a field of its own type,
also under a list, 'NonEmpty', 'Set' or 'Maybe': like the @Neg Expr@ in @data Expr =
Lit Int | Neg Expr@ and the @[Tree]@ in @data Tree = Node [Tree]@, since a
container carries its element's 'Property'. \"Mutually recursive\" means the same
through another type whose 'Property' is 'Structural', or through a polymorphic
type instantiated at the recursive one. Nothing in the types prevents it, so
choose another 'Property' for the recursive type: @type Property Expr = Void@, as
above, which drops the properties of its other fields too, or a custom 'Property'
that does not contain @'Structural' Expr@. A cycle is what never ends: two
fields of one type are fine.

To keep the other properties of a recursive type, name the properties of its
non-recursive fields and check the recursive field by hand with 'everyHolds'.
For

@
data Section = Section { heading :: Title, subsections :: [Section] }
@

the properties of the @Title@ are the properties of the @Section@, with a
'Finite' instance, and they are checked on the heading and, recursively, on every
subsection:

@
instance Surveyable Section where
  type Property Section = Property Title
  propertyHolds s p = propertyHolds (heading s) p && everyHolds (subsections s) p
  describeProperties _ p = describeProperties (Proxy \@Title) p
@

The check ends on a finite tree, since 'everyHolds' recurses into the
subsections, each one smaller. @type Property Section = Property Title@ needs
@UndecidableInstances@ in your module, as the type family application is no smaller
than the left-hand side; naming
the concrete property type of @Title@ instead, say
@type Property Section = TitleProperty@, does not.

== A field of a type you do not own

@type Property X = Structural X@ fails with an @All2 Surveyable@ error if one of
the fields has no 'Describable' and 'Surveyable' instance, as happens for a field
of a third-party type. Either give the enclosing type another 'Property', such
as @Void@ above, or give the field type — or a @newtype@ around it, to avoid an
orphan instance — its own instances.
-}
class (Describable a, Finite (Property a)) => Surveyable a where
  {- | The type of properties for @a@.  Must have a 'Finite'
    instance.  __There is no default: every instance states its @type Property@__,
    and leaving it out is a compile error (see the header of 'Surveyable' for the
    errors GHC gives). The common choices:

    * @type Property X = Void@: no properties at all. Nothing else to write, and
      no @generics-sop@ instances needed.

    * @type Property X = 'Structural' X@: the properties of @X@'s fields, each
      field type's own 'Property'. Nothing else to write. Needs @SOP.Generic@ and
      @SOP.HasDatatypeInfo@ for @X@ and a 'Surveyable' instance for every field
      type. Caveat: a type recursive through its own structural properties never
      ends, see the recursion section of the header.

    * @type Property X = Either XProperty ('Structural' X)@: a type's own
      properties next to those of its fields. Caveat: 'propertyHolds' and
      'describeProperties' are written by hand, four equations in all, the
      @Right@ ones handing over to 'structuralHolds' and 'describeStructural'.
      The same caveat about recursion as for 'Structural'.

    * An enumeration of your own, @data XProperty = … deriving (Bounded, Enum,
      Universe, Finite, Eq, Ord, Show)@, with @type Property X = XProperty@.
      Caveat: the fields' properties are not included, and both methods are
      written by hand.

    * Forwarding a field's properties, @type Property Section = Property Title@.
      Caveat: it needs @UndecidableInstances@ (naming @Title@'s concrete property
      type does not), and the methods are written by hand, @propertyHolds@
      checking the field (and, for a recursive type, the recursive fields with
      'everyHolds').

    * A 'Foldable' container: @deriving via ('Every' \"Every entry\" Roster a)@,
      which has the element's 'Property'. Caveat: it needs @Surveyable a@ for the
      element, and @DerivingVia@ and @UndecidableInstances@.

    An instance that only wants 'examples' writes @type Property@ and @examples@,
    the methods coming from 'StandardProperties' as long as 'Property' is 'Void'
    or 'Structural'.
  -}
  type Property a :: Type

  {- | Check whether the property holds.

   Returns 'True' if the property holds for the value, 'False' if it is
    violated.  Used for validation after parsing.

    Defaults to checking the 'Structural' properties of the fields if 'Property'
    is @'Structural' a@, and to nothing at all if it is 'Void'. For any other
    'Property' there is no default, and the method must be written.

    Hint: If you have an "informal" property (like "is an english name")
    that can't be easily checked with code,
    you can still include it in 'describeProperties' and return 'True' here.
  -}
  propertyHolds :: a -> Property a -> Bool
  default propertyHolds :: (StandardProperties (Property a) a) => a -> Property a -> Bool
  propertyHolds = defaultHolds

  {- | Human-readable description of a property, used in the prompt.
    May be 'Nothing' if you don't wish to describe it; the property is
    still checked by 'propertyHolds'. A checked but undescribed property is
    enforced without the model being told what to aim at, so it can meet it
    only by chance, and the error does not say which property broke: describe
    every property you check, unless the model meets it anyway.

    Defaults like 'propertyHolds': for the 'Structural' properties of the fields
    if 'Property' is @'Structural' a@, for no property at all if it is 'Void',
    and otherwise the method must be written.
  -}
  describeProperties :: Proxy a -> Property a -> Maybe Text
  default describeProperties :: (StandardProperties (Property a) a) => Proxy a -> Property a -> Maybe Text
  describeProperties = defaultDescribe

  {- | Representative example values.  Included in the prompt when non-empty, in the
    order given: they are neither sorted nor deduplicated, and need no 'Ord'.
    Defaults to none.
  -}
  examples :: Proxy a -> [a]
  examples _ = []

{- | Every property of @a@ as a 'PropertyCheck': one entry per element of
'universeF' of 'Property', in its order, with the description
'describeProperties' gives and the check 'propertyHolds' makes.
-}
typeProperties :: forall a. (Surveyable a) => [PropertyCheck a]
typeProperties = [PropertyCheck (describeProperties (Proxy @a) p) (`propertyHolds` p) | p <- universeF @(Property a)]

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

-- A container has the properties of its element, told with a label and checked
-- on every element, see "Lifting element properties into containers" below.

{- | The properties of @a@, checked on every element and told as \"Every
element: …\".
-}
deriving via (Every "Every element" [] a) instance (Surveyable a) => Surveyable [a]

{- | The properties of @a@, checked on every element and told as \"Every
element: …\".
-}
deriving via (Every "Every element" NonEmpty a) instance (Surveyable a) => Surveyable (NonEmpty a)

{- | The properties of @a@, checked on every element and told as \"Every
element: …\".
-}
deriving via (Every "Every element" Set a) instance (Surveyable a) => Surveyable (Set a)

{- | The properties of @a@, checked on the value if there is one and told as
\"If present: …\". Vacuously true for 'Nothing'.
-}
deriving via (Every "If present" Maybe a) instance (Surveyable a) => Surveyable (Maybe a)

-- ** Generically

-- | Forwards to the wrapped type, see the 'Describable' instance.
instance (Surveyable a, ToJSON (Generically a)) => Surveyable (Generically a) where
  type Property (Generically a) = Property a
  propertyHolds (Generically x) = propertyHolds x
  describeProperties _ = describeProperties (Proxy @a)
  examples _ = Generically <$> examples (Proxy @a)

-- * Property machinery

-- ** Lifting element properties into containers

{- | A container @f a@ with the properties of its element @a@, told to the model
under the label @label@ and checked on every element: @'Property' ('Every'
label f a) = 'Property' a@. This is the one definition, over any 'Foldable',
behind the 'Surveyable' instances of @[a]@, @'NonEmpty' a@, @'Set' a@ and
@'Maybe' a@, each one @deriving via@ this type with its label (\"Every element\",
for 'Maybe' \"If present\"): a container is checked vacuously when empty (and
for 'Nothing').

A 'Foldable' container of your own gets the same with one line (and the
language extensions @DerivingVia@ and @UndecidableInstances@, the latter
because the associated 'Property' is a type family application), here for a
@Roster@ that wraps a list (and has a 'Describable' instance of its own):

@
deriving via (Every \"Every entry\" Roster a) instance (Surveyable a) => Surveyable (Roster a)
@

A field of type @Roster X@ then contributes @X@'s properties, under that label,
to the structural properties of the type that holds it, if its 'Property' is
'Structural'. Enumerating the
'Universe' of such a container's 'Property' ends exactly when the element's
does, so a type recursive through it must not have 'Structural' as its
'Property': see the recursion section in 'Surveyable', which also says what to
write instead.

'everyHolds' and 'describeEvery' are the pair behind it, and also serve a
hand-written lift into a custom 'Property', as one side of an 'Either', when a
container has properties of its own besides those of its elements:

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
instance's own left-hand side. Do not combine a lift like this with
@'Structural' X@ for a container field of @X@ whose element properties it
lifts: the structural properties already tell and check them, so they would be
told and checked twice.

@runPrompt@ (from the @shroom@ package) only checks the properties of the
top-level value, and 'description' only lists those: the properties of a
container's elements are told and checked because the container's own
'Property' is the element's.
-}
newtype Every (label :: Symbol) (f :: Type -> Type) (a :: Type) = Every (f a)

{- | Transparent: it describes the container @f a@, but is never shown to the
model, since only the type a 'description' is asked for (here the derived
instance's own type) is described. It exists as the superclass of the
'Surveyable' instance.
-}
instance (Describable (f a), Typeable label, Typeable f, Typeable a) => Describable (Every label f a) where
  describeType _ = describeType (Proxy @(f a))
  referencedTypes _ = referencedTypes (Proxy @(f a))
  hasOwnSection _ = False
  describedIdentity _ = describedIdentity (Proxy @(f a))

deriving newtype instance (ToJSON (f a)) => ToJSON (Every label f a)

-- | The properties of @a@ for each element, vacuously true for an empty container.
instance (KnownSymbol label, Foldable f, Surveyable a, Describable (Every label f a)) => Surveyable (Every label f a) where
  type Property (Every label f a) = Property a
  propertyHolds (Every xs) = everyHolds xs
  describeProperties _ = describeEvery (T.pack (symbolVal (Proxy @label))) (Proxy @a)

{- | Check that one of an element type's properties holds for every element
of a container, vacuously for an empty one (and for 'Nothing'). The partner of
'describeEvery', and what 'Every' uses.
-}
everyHolds :: (Foldable f, Surveyable a) => f a -> Property a -> Bool
everyHolds xs p = all (`propertyHolds` p) xs

{- | Describe an element's property as a property of every element of a
container, the partner of 'everyHolds'. The label is joined to the element's
own description with a colon, e.g.
@describeEvery \"Every speaker\"@ turns \"The speaker's last name must not be
empty.\" into \"Every speaker: The speaker's last name must not be empty.\".

Returns 'Nothing' when the element has no description for the property, so a
container tells nothing where the element tells nothing. The 'Surveyable'
instances of the containers use the label \"Every element\" and, for 'Maybe',
\"If present\", through 'Every'.
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
@

'propertyHolds' and 'describeProperties' then default to 'structuralHolds' and
'describeStructural', so nothing else is written; they are only named by hand as
the @Right@ half of an 'Either' (see 'Surveyable').

(For this the type needs the @generics-sop@ classes 'Generic' and
'HasDatatypeInfo', both derivable from "GHC.Generics"'s own @Generic@.)

It is a newtype, not a type family, so that naming it as a 'Property' needs
no @UndecidableInstances@ in the user's module.

Its 'Universe' lists every field property of every constructor, in
constructor and then field order. A constructor without fields, and a field
whose own 'Property' is empty, contribute nothing.

A type recursive through its own structural properties (a field of its own
type, also under a container, or through another type's structural properties)
has no finite 'Universe', although the 'Finite' instance claims one: with
'Structural' as its 'Property', 'description' and shroom's retry loop run forever
before any request is sent. See the recursion section in 'Surveyable' for what
to write instead.
-}
newtype Structural a = Structural (NS (NS PropertyOf) (Code a))

deriving instance (Eq (NS (NS PropertyOf) (Code a))) => Eq (Structural a)

deriving instance (Show (NS (NS PropertyOf) (Code a))) => Show (Structural a)

instance (All2 Surveyable (Code a)) => Universe (Structural a) where
  universe = Structural <$> universeConstructors

{- | The structural properties are finitely many, as each field's 'Property' is
'Finite' (a superclass of 'Surveyable'), unless the type recurs through them: see
'Structural'.
-}
instance (All2 Surveyable (Code a)) => Finite (Structural a)

universeConstructors :: forall xss. (All2 Surveyable xss) => [NS (NS PropertyOf) xss]
universeConstructors = case sList :: SList xss of
  SNil -> []
  SCons -> (Z <$> universeFields) ++ (S <$> universeConstructors)

universeFields :: forall xs. (All Surveyable xs) => [NS PropertyOf xs]
universeFields = case sList :: SList xs of
  SNil -> []
  SCons -> (Z . PropertyOf <$> universeF) ++ (S <$> universeFields)

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

{- | The property types whose 'propertyHolds' and 'describeProperties' an
instance need not write, chosen by the instance's 'Property' type @p@: nothing
for 'Void', the structural properties of the fields for @'Structural' a@. For any
other 'Property' there is no instance, and both methods must be written.

This class is an implementation detail of 'Surveyable''s method defaults, and is
exported only because its name shows up in compile errors, of two kinds:

* @No instance for ‘StandardProperties (Property X) X’@, with @Property X@
  __unreduced__ (and, next to it, @No instance for ‘Finite (Property X)’@): the
  instance of 'Surveyable' for @X@ states no @type Property@. Write one, see the
  common choices in 'Property' and in the header of 'Surveyable'.
* @No instance for ‘StandardProperties XProperty X’@, with a concrete custom type
  as the first argument: the instance names its own @type Property X =
  XProperty@ and leaves 'propertyHolds' or 'describeProperties' out. A custom
  'Property' must define both.

Do not write instances of this class.
-}
class StandardProperties p a where
  -- | The default 'propertyHolds'.
  defaultHolds :: a -> p -> Bool

  -- | The default 'describeProperties'.
  defaultDescribe :: Proxy a -> p -> Maybe Text

-- | No properties: nothing to check, nothing to describe.
instance StandardProperties Void a where
  defaultHolds _ = absurd
  defaultDescribe _ = absurd

-- | The properties of the fields, see 'Structural'.
instance (Generic a, HasDatatypeInfo a, All2 Surveyable (Code a)) => StandardProperties (Structural a) a where
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
      | null (examples p) = []
      | otherwise =
          "Example valid JSON responses:"
            : (toStrict . decodeUtf8 . encode <$> examples p)

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
