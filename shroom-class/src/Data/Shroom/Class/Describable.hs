{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}

{- | The 'Describable' class: a type's one-sentence description, the types it
refers to, and the 'Describable' instances for the base types, the containers
and 'Generically'.

"Data.Shroom.Class" re-exports everything here. The instances live with the
class, so that none of them is an orphan.
-}
module Data.Shroom.Class.Describable where

-- base
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.Proxy (Proxy (..))
import Data.Typeable (TypeRep, Typeable, typeRep)
import GHC.Generics (Generically (..))
import Numeric.Natural (Natural)

-- containers
import Data.Set (Set)
import Data.Set qualified as S

-- text
import Data.Text (Text)

-- aeson
import Data.Aeson (ToJSON)

-- time
import Data.Time (Day, UTCTime)

-- * Describable

-- ** The class

{- | Attach a one-sentence description to a type.

Minimal complete definition: 'describeType'.

Use @$(deriveDescribable ''MyType)@ from "Control.Monad.Prompt.TH" to derive
this automatically from all Haddock comments on the type declaration.

'Typeable' is a superclass so that 'Data.Shroom.Class.Surveyable.description' can tell types apart when it
lists each reachable type once (see 'referencedTypes'). Every concrete type is
'Typeable' automatically, and a polymorphic instance gets it from the
'Describable' context of its type parameter, so no instance needs to change.

== What reaches the model

'Data.Shroom.Class.Surveyable.description' sends the requested type's own 'describeType', and then, in a
section of their own, the 'describeType' of every type it can reach through
'referencedTypes', each once. A type that wants its field types documented to
the model lists them in 'referencedTypes'; @deriveDescribable@ does this for
you. __A hand-written instance that does not write 'referencedTypes' contributes
no reachable types__: the types it refers to are not described to the model.
-}
class (ToJSON a, Typeable a) => Describable a where
  {- | A one- or two-sentence description of what the type represents,
    and what the constructors and fields mean.
  -}
  describeType :: Proxy a -> Text

  {- | The types whose descriptions the model also needs in order to produce
    an @a@: typically the types of its fields. 'Data.Shroom.Class.Surveyable.description' follows these
    transitively and lists each type once, so an entry may refer back to the
    type itself or to another entry without harm.

    Build an entry for a field type @t@ with @'reach' \@t@, which takes the
    container and base types out and passes their elements through. Defaults
    to the empty list, so a hand-written instance that does not write this
    method contributes no reachable types.
  -}
  referencedTypes :: Proxy a -> [Described]
  referencedTypes _ = []

  {- | Whether the type gets a section of its own in the list of reachable
    types. Defaults to 'True', so a hand-written instance is listed. The
    base types ('Text', 'Int', …) and the containers (@[a]@, 'Maybe', …) set it
    to 'False': they need no section, as their 'describeType' is a sentence
    that the types using them already quote.
  -}
  hasOwnSection :: Proxy a -> Bool
  hasOwnSection _ = True

  {- | The identity of the type that 'Data.Shroom.Class.Surveyable.description' reports on, by which
    'reachableTypes' keeps that type out of its own list of reachable types.
    Defaults to the type's own 'TypeRep'. A transparent wrapper (@'Generically' X@,
    @DescriptionPrompt X@) forwards to the wrapped type's, because @description@ runs at
    the wrapper while the types that refer back to @X@ name @X@ itself.

    == Writing a transparent wrapper

    A transparent wrapper @W a@ stands for @a@ without being a type of its own to
    the model. Its 'Describable' instance must forward 'describeType', and the
    three methods beyond it:

    * 'referencedTypes' forwards to @a@'s (@referencedTypes _ = referencedTypes (Proxy \@a)@),
      not @'reach' \@a@, which would list @a@ under itself;
    * 'hasOwnSection' is 'False', so the wrapper gets no section;
    * 'describedIdentity' forwards to @a@'s. Forget this one and 'reachableTypes'
      lists the requested type under itself.

    There are two such wrappers: @'Generically' a@ in this module, and
    @DescriptionPrompt a@ in "Control.Monad.Prompt.Promptable" of the @shroom@
    package. Keep the two in step.
  -}
  describedIdentity :: Proxy a -> TypeRep
  describedIdentity = typeRep

-- ** Referenced types

{- | A type of which only the 'Describable' instance is known, to put types of
different kinds into one list: what 'referencedTypes' returns.
-}
data Described = forall t. (Describable t) => Described (Proxy t)

{- | The entries a field of type @t@ contributes to 'referencedTypes': @t@ itself
if it has a section of its own ('hasOwnSection'), otherwise whatever @t@ itself
references. So a field of type @[Speaker]@ or @Maybe Speaker@ contributes
@Speaker@, and a field of type @Text@ contributes nothing:

@
referencedTypes _ = reach \@Person <> reach \@[Address] <> reach \@Text
@
-}
reach :: forall t. (Describable t) => [Described]
reach
  | hasOwnSection (Proxy @t) = [Described (Proxy @t)]
  | otherwise = referencedTypes (Proxy @t)

-- | Every type reachable from @a@ through 'referencedTypes' that has a section, see 'Data.Shroom.Class.Surveyable.description'.
reachableTypes :: forall a. (Describable a) => Proxy a -> [Described]
reachableTypes p = go (S.singleton (describedIdentity p)) (referencedTypes p)
  where
    -- Breadth first: the whole frontier is emitted before anything it references.
    go _ [] = []
    go seen frontier =
      let (seen', visited) = foldl' visit (seen, []) frontier
          visited' = reverse visited
       in filter hasSection visited' <> go seen' (concatMap children visited')
    visit (seen, acc) d@(Described q)
      | typeRep q `S.member` seen = (seen, acc)
      | otherwise = (S.insert (typeRep q) seen, d : acc)
    hasSection (Described q) = hasOwnSection q
    children (Described q) = referencedTypes q

-- * Base types

-- Instances for common base types, so that a type with a field of one of them
-- can derive its own instances. Each describes the type in a short plain
-- sentence.
--
-- They live in the module of their class, not a sibling: there they are not orphans,
-- and modules importing only "Data.Shroom.Class" see them.

-- ** Scalars

instance Describable Text where
  describeType _ = "A piece of text."
  hasOwnSection _ = False

instance Describable Int where
  describeType _ = "A whole number."
  hasOwnSection _ = False

instance Describable Integer where
  describeType _ = "A whole number."
  hasOwnSection _ = False

instance Describable Int64 where
  describeType _ = "A whole number."
  hasOwnSection _ = False

instance Describable Word where
  describeType _ = "A non-negative whole number."
  hasOwnSection _ = False

instance Describable Natural where
  describeType _ = "A non-negative whole number."
  hasOwnSection _ = False

instance Describable Double where
  describeType _ = "A number, possibly with a fractional part."
  hasOwnSection _ = False

instance Describable Float where
  describeType _ = "A number, possibly with a fractional part."
  hasOwnSection _ = False

instance Describable Bool where
  describeType _ = "A truth value: true or false."
  hasOwnSection _ = False

instance Describable Char where
  describeType _ = "A single character."
  hasOwnSection _ = False

instance Describable UTCTime where
  describeType _ = "A point in time, in UTC."
  hasOwnSection _ = False

instance Describable Day where
  describeType _ = "A calendar date."
  hasOwnSection _ = False

-- ** Containers

{- | A list, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'Data.Shroom.Class.Surveyable.everyHolds' and 'Data.Shroom.Class.Surveyable.describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable [a] where
  describeType _ = "A list of values of the following type: " <> describeType (Proxy @a)
  referencedTypes _ = reach @a
  hasOwnSection _ = False

{- | A non-empty list, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'Data.Shroom.Class.Surveyable.everyHolds' and 'Data.Shroom.Class.Surveyable.describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable (NonEmpty a) where
  describeType _ = "A non-empty list of values of the following type: " <> describeType (Proxy @a)
  referencedTypes _ = reach @a
  hasOwnSection _ = False

{- | A set, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'Data.Shroom.Class.Surveyable.everyHolds' and 'Data.Shroom.Class.Surveyable.describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable (Set a) where
  describeType _ = "A set of distinct values of the following type: " <> describeType (Proxy @a)
  referencedTypes _ = reach @a
  hasOwnSection _ = False

{- | An optional value, described through its element type.

Does not lift the element's properties: they are neither described nor checked
for the container. Use 'Data.Shroom.Class.Surveyable.everyHolds' and 'Data.Shroom.Class.Surveyable.describeEvery' by hand for now; a
derivable version is tracked in
<https://github.com/turion/shroom/issues/18 issue #18>.
-}
instance (Describable a) => Describable (Maybe a) where
  describeType _ = "A value that may be absent (null), and otherwise of the following type: " <> describeType (Proxy @a)
  referencedTypes _ = reach @a
  hasOwnSection _ = False

-- ** Generically

{- | Forwards to the wrapped type.

These two instances (this one and the one for 'Data.Shroom.Class.Surveyable.Surveyable') exist so that
@deriving via 'Generically' X instance Promptable X@ (from the @shroom@ package)
has superclasses to build on: a @deriving via@ coerces only the class's own
method, and the superclass instances of @'Generically' X@ are what GHC then
needs. At runtime the @X@ instances are the ones in use. Each lives with
its class, so neither is an orphan.

'ToJSON' for @'Generically' a@ is @aeson@'s own, which is always the
"GHC.Generics"-based encoding with default options, hence the extra context
(satisfied wherever @a@ derives 'GHC.Generics.Generic').
-}
instance (Describable a, ToJSON (Generically a)) => Describable (Generically a) where
  describeType _ = describeType (Proxy @a)

  -- Like a container, but transparent: @a@ is the type being described, so it
  -- must not appear as a section of its own under it ('reach' would list it).
  -- The three methods below are the contract of a transparent wrapper, see
  -- 'describedIdentity'; 'DescriptionPrompt' in @shroom@ is the other one.
  referencedTypes _ = referencedTypes (Proxy @a)
  hasOwnSection _ = False
  describedIdentity _ = describedIdentity (Proxy @a)
