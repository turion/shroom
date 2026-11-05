# Revision history for shroom-class

## Unreleased

* **Breaking: a `Property` type must be `Finite`, not only `Universe`.** `Surveyable`'s superclass
  is now `Finite (Property a)` (from `Data.Universe.Class`, `universe-base`) instead of `Universe
  (Property a)`, and `typeProperties` enumerates with `universeF`. Migrate each property type by
  adding `Finite` to its deriving clause beside `Universe` (`deriving (Bounded, Enum, Universe,
  Finite, Eq, Ord, Show)`, or `deriving anyclass (Universe, Finite)`); `Finite` needs no method, as
  `cardinality` has a default. `Void` and `Either` already have one, and `Structural` gets one
  beside its `Universe`. Why: a `Finite` instance is a declaration that the universe eventually
  ends, which is what shroom assumes when it tells and checks every property; a type recursive
  through a container whose properties never end is the case this is meant to make visible. One
  visible change: universe-base's `Finite (Either a b)` lists every `Left` before every `Right`,
  where `universe` interleaves them, so for the documented `Either XProperty (Structural X)`
  property type a type's own properties now come before those of its fields (for
  `ConferenceSchedule`, its "Every slot" line moves from second to last), in the prompt and in the
  order of repair. Every other order is unchanged.
* **Breaking: `examples` returns a list, `examples :: Proxy a -> [a]` (it was `Set a`).** Migrate
  each definition by dropping the `Set`: `S.fromList xs` becomes `xs` and `S.singleton x` becomes
  `[x]`. The examples are now shown in the order given, neither sorted nor deduplicated (a `Set`
  sorted them by `Ord`; keep that order to leave the rendered prompt as it was), and they need no
  `Ord`. Why: a `Set` is nominal in its element and cannot be coerced, so a newtype's `Surveyable`
  instance could not be derived by `deriving via`; a list can. It is also the first half of
  connect's R7, the request of one example at a time, which is still to come.
* **`Data.Shroom.Class` is split into a module per class; it still exports everything.** The new
  `Data.Shroom.Class.Describable` holds the class `Describable`, its helpers (`Described`, `reach`,
  `reachableTypes`) and every `Describable` instance; `Data.Shroom.Class.Surveyable` holds the class
  `Surveyable`, `typeProperties`, every `Surveyable` instance, `everyHolds` and `describeEvery`,
  `Structural` and its machinery, `StandardProperties`, and `description`. `Data.Shroom.Class` now
  defines nothing and re-exports these two modules and `Data.Shroom.Class.PropertyCheck`, so
  every name it exported before is still exported from it and no import has to change. Nothing
  behaves differently. One visible difference: the name in the compile error for a custom
  `Property` that leaves out a method (see below) is now `Data.Shroom.Class.Surveyable.$dm…`.
* **New: a property as a value, `PropertyCheck a`, in the new module `Data.Shroom.Class.PropertyCheck`
  (re-exported by `Data.Shroom.Class`).** `PropertyCheck {description :: Maybe Text, holds :: a ->
  Bool}` is one property: what the model is told (`Nothing`: left out of the prompt's property list)
  and the check. The module has `NoFieldSelectors`, so the field `description` does not clash with
  the `description` function; read a field with `OverloadedRecordDot` (`c.description`, `c.holds`).
  Record construction next to the `description` function needs `GHC2024` or
  `DisambiguateRecordFields`; under `Haskell2010` or `GHC2021` build it positionally,
  `PropertyCheck (Just "…") check`. Also new: `typeProperties :: Surveyable a => [PropertyCheck a]`,
  one entry per element of `universeF @(Property a)` with `describeProperties`'s description and the
  check `propertyHolds`, and `propertyLines :: [PropertyCheck a] -> [Text]`, the `"- <description>"`
  lines of a list of properties, leaving out an undescribed one. `description` draws its property
  section from `propertyLines typeProperties`; its output is unchanged.
* **New: `description` also describes every type reachable from the requested one, once each.**
  After the examples it appends a section ("The types above refer to the following types:") with,
  for each type reachable through the new `Describable` method `referencedTypes`, a header line
  naming the type (`Choice Fuel`) and its `describeType`. Each type appears once (told apart by
  `TypeRep`, so `Choice Fuel` and `Choice Phase` are two types), breadth first from the requested
  type, never the requested type itself, and recursive types terminate. A type with nothing
  reachable renders exactly as before. New API: `referencedTypes :: Proxy a -> [Described]` (default
  `[]`), `hasOwnSection :: Proxy a -> Bool` (default `True`; `False` for the base types and the
  containers `[a]`, `NonEmpty a`, `Maybe a`, `Generically a`, which pass their element's entry
  through), the existential `Described`, and `reach @t`, the entry a field of type
  `t` contributes. **A hand-written `Describable` instance that does not write `referencedTypes`
  contributes no reachable types**, so the types it refers to still do not reach the model.
* **Breaking: `deriveDescribable` also generates `referencedTypes`, and so needs every field type
  to be `Describable`.** The generated instance lists, for every field of every constructor
  (record and positional alike), the field type's `reach`, so that the types the fields refer to
  are described in the prompt text once each. Migrating a model package:
  - (a) **Every field type of a derived type needs a `Describable` instance**, from an earlier
    splice or from the same one; a field type without one is a compile error (`No instance for
    Describable …`), by design: a silent skip could not see instances spliced later in the module
    and would drop types without a word. Reorder the splices so that a field type comes before the
    types that use it, and derive (or write) an instance for field types that had none. Mutually
    recursive types are derived by a single splice over the list,
    `$(concat <$> traverse deriveDescribable [''A, ''B])`; a self-recursive type needs nothing
    special.
  - (b) **A parameterised type now derives**: `data Choice a = …` gets
    `instance (Describable a) => Describable (Choice a)`, and `Choice Fuel` makes `Fuel`
    reachable. Switch a hand-written instance for such a wrapper to `deriveDescribable`, or write
    `referencedTypes _ = reach @a` in it; otherwise the element types silently get no section. A
    type variable of kind `Type` gets a `Describable` constraint; a variable of any other kind (a
    `Symbol` or `Nat` unit, a dimension, or an unannotated phantom parameter, which `GHC2024` makes
    poly-kinded) can never be a field's type, so it is annotated with its kind in the instance head
    and gets only `Typeable`, plus `Typeable` for each kind variable of its kind:
    `newtype Tagged a = Tagged Double` derives
    `instance (Typeable k, Typeable a) => Describable (Tagged (a :: k))`. The splicing module needs
    `TypeApplications` and `ScopedTypeVariables` (both in `GHC2021`/`GHC2024`).
  - (c) **Compile the model package with `-haddock`**: the descriptions come from the Haddock
    comments, and a type without one is a compile-time error, as before.
  - (d) **Check the result** with `T.putStrLn (description (Proxy @T))` in `cabal repl`: the types
    your model should know about each appear once under "The types above refer to the following
    types:".

  A hand-written instance that uses `deriveDescribableExp` for `describeType` must now also write
  `referencedTypes` itself; `deriveDescribableExp` returns the `describeType` body only.
* **New: `deriveDescribable` collects the docs of positional constructor fields, and no longer drops
  a single constructor's own doc.** A documented positional argument is rendered like a record
  field, labelled by its 1-based position: `- field 1: The first point.`, indented under its
  constructor in a sum, directly under the type for a single constructor; an undocumented one adds
  nothing. And `newtype Bounds = Bounds Int -- ^ The largest allowed value.` now renders that
  constructor doc as a line after the type's own (a single-constructor type used to render its
  fields only). Which Haddock layout documents what (GHC's rule for H98 constructors): a
  **one-line** trailing `ByFuel X -- ^ doc` documents the *constructor* — it already rendered as
  `- ByFuel: doc` and still does. An *argument* is documented only if the constructor has a
  leading `-- |` doc, or each argument stands on a line of its own with its own comment, in either
  the leading form (`-- | doc` above the argument, which is what fourmolu rewrites trailing
  comments into) or the trailing form (`-- ^ doc` below it). If your model package documented
  positional arguments and they did not reach the model, check it against this; the descriptions
  of types that did not document arguments or a single constructor are unchanged.
* New: `describedIdentity :: Proxy a -> TypeRep`, a `Describable` method (default: the type's own
  `typeRep`) naming the type that `description` reports on, so the type is not listed under itself.
  `Generically a` and `DescriptionPrompt a` forward to `a`'s, which fixes a recursive `X`
  promoted via `deriving via Generically X`/`DescriptionPrompt X` being listed under itself. A
  hand-written transparent wrapper forwards it the same way.
* **Breaking: `Typeable` is now a superclass of `Describable`.** Every concrete type has it, and a
  polymorphic instance gets it from the `Describable a` context of its parameter, so most instances
  compile unchanged. An instance whose type is indexed by a parameter that is not itself
  `Describable` (say `instance Describable (Report tools)` with `tools :: [Type]`) needs
  `Typeable tools` in its context.
* New: `Describable` and `Surveyable` instances for `Int64` and `Word`.
* New: `Describable` and `Surveyable` instances for `Set a` (context `Describable a`), shaped like
  the `[a]` and `NonEmpty a` ones: described through the element type, with no section of its own,
  so `reach @(Set X)` is `X`'s entry. No `Ord a` is needed here; only `shroom`'s `Promptable` instance
  asks for it.
* **Breaking: `Surveyable`'s `Property` has no default; state it.** It used to default to `Void`, so
  a bare `instance Surveyable X` had no properties. Now an instance that leaves out `type Property`
  does not compile, with these errors (the second one for each of `propertyHolds` and
  `describeProperties`):

  ```
  No instance for ‘Finite (Property X)’
    arising from the superclasses of an instance declaration
  No instance for ‘StandardProperties (Property X) X’
    arising from a use of ‘Data.Shroom.Class.Surveyable.$dmpropertyHolds’
  ```

  `Property X` stays unreduced in both. Migrate each such instance by writing its `Property`:
  - **`type Property X = Void`** keeps the old behaviour (no properties), with no methods to write and
    no `generics-sop` classes. Use this for a field of a type you do not own, which has no
    `Describable`/`Surveyable` instance, or when you do not want the `generics-sop` instances.
  - **`type Property X = Structural X`** gives `X` the properties of its fields — each field type's
    own `Property` — described in the prompt as `Field name: …` and checked on the parsed value;
    `propertyHolds`/`describeProperties` default to `structuralHolds`/`describeStructural`, so no
    methods are written either. It needs `SOP.Generic` and `SOP.HasDatatypeInfo` for `X`
    (`deriving anyclass (…, SOP.Generic, SOP.HasDatatypeInfo)`, with
    `import Generics.SOP qualified as SOP` and `generics-sop` in your `build-depends`) and a
    `Surveyable` instance for every field type (otherwise an `All2 Surveyable` error); if none of
    the fields has properties, nothing in the prompt changes.

  The `Property` Haddock and the `Surveyable` header list the other common choices with their
  caveats: `Either XProperty (Structural X)` with hand-written methods, an enumeration of your own
  (`deriving (Bounded, Enum, Universe, Finite)`), forwarding a field's properties (`type Property
  Section = Property Title`) and a `Foldable` container through `Every`.
* **A directly or mutually recursive type must not have `Structural X` as its `Property`: its
  `universe` never ends.**
  `universe @(Structural X)` has no end for a type with a field of its own type, also under a list,
  `NonEmpty`, `Set` or `Maybe` (`data Expr = Lit Int | Neg Expr`, `data Tree = Node [Tree]`: a
  container carries its element's `Property`, see "Container fields" below), or that reaches itself
  through another type whose `Property` is `Structural`, or through a polymorphic type instantiated
  at the recursive one (`data Grouping = Grouping (NonEmpty (Branch Cond Grouping))`), although the
  `Finite (Structural X)` instance claims it is finite. `description` and shroom's retry loop (which
  enumerate the properties) then run forever, in `shroom` before any request is sent and on every
  backend. Nothing in the types stops it. Instead give the recursive type
  `type Property X = Void`, which drops the properties of its other fields too, or a custom `Property`
  that does not contain `Structural X`. Two fields of one structural type are no recursion. To keep
  the type's other properties, name the properties of its non-recursive fields and check the
  recursive field with `everyHolds`: for
  `data Section = Section {heading :: Title, subsections :: [Section]}`, write
  `type Property Section = Property Title` (which needs `UndecidableInstances`; the concrete
  `TitleProperty` does not) with
  `propertyHolds s p = propertyHolds (heading s) p && everyHolds (subsections s) p`; the `Surveyable`
  Haddock shows it.
* **Breaking: the new base-type instances can overlap yours.** `Describable` and `Surveyable`
  instances now exist for `Text`, `Int`, `Integer`, `Natural`, `Double`, `Float`, `Bool`, `Char`,
  `UTCTime`, `Day`, `[a]`, `NonEmpty a` and `Maybe a`. A downstream package that already defines one
  of them, say `instance Describable [Foo]` or `instance Surveyable Text`, now gets a
  duplicate-instance or overlapping-instances error; delete its instance, or wrap the type in a
  `newtype` of your own.
* **Breaking: a custom `Property` now needs both `propertyHolds` and `describeProperties`.** They used
  to default to `True` and `Nothing`; now they default only for `Void` (nothing) and `Structural a`
  (the fields), so an instance with `type Property X = XProperty` that leaves one out fails to
  compile, for example with

  ```
  No instance for ‘StandardProperties XProperty X’
    arising from a use of ‘Data.Shroom.Class.Surveyable.$dmdescribeProperties’
  ```

  (`$dmpropertyHolds` when `propertyHolds` is the one missing; note the concrete `XProperty`, where
  a missing `type Property` leaves `Property X` unreduced, see the entry above). Write the method;
  for an informal property, `propertyHolds _ _ = True` switches it off. `describeProperties _ _ =
  Nothing` only keeps it out of the prompt, and `shroom` still enforces it (see its changelog's
  behaviour-change entry on undescribed properties). `StandardProperties` is the class behind those
  defaults, exported for its name's sake (renamed from `PropertyDefault`, which was never
  released).
* **A custom `Property` does not include the fields' properties.** Adding one property of your own
  to a type that had `Structural X` silently drops its fields' properties from the prompt and from
  validation. To keep both, combine them: `type Property X = Either XProperty (Structural X)`, with
  `structuralHolds` and `describeStructural` on the `Right` side; the `Surveyable` Haddock has the
  four equations.
* **Breaking: container fields have their elements' properties, and the container instances need
  `Surveyable a`.** The `Surveyable` instances of `[a]`, `NonEmpty a`, `Set a` and `Maybe a` now have
  `Property (f a) = Property a`: each property of the element is told to the model under a label
  ("Every element: …", for `Maybe` "If present: …") and checked on every element, vacuously for
  `Nothing` and an empty container. So under `Structural X`, a
  `[Speaker]` or `Maybe Name` field now contributes its element's properties to the type's
  `description` and checks, as `Field speakers: Every element: …`. **A reply that was accepted
  before can now fail a property of an element and get a repair turn.** The instances' context is
  now `Surveyable a` instead of `Describable a`, so an element type that has only a `Describable`
  instance (the field type of a `Structural` type, or the element of a `[Foo]` field) needs a
  `Surveyable` instance as well (`type Property Foo = Void` is enough). One definition, the newtype
  `Every label f a` over any `Foldable` `f` (with `everyHolds` and `describeEvery`), backs the four
  by `deriving via`, and a container of your own gets its element's properties with one line,
  `deriving via (Every "Every entry" Roster a) instance (Surveyable a) => Surveyable (Roster a)`
  (needs `DerivingVia` and `UndecidableInstances`, as the associated `Property` is a type family
  application). Nothing needs lifting by hand any more: a hand-written lift of such a field combined
  with `Structural X` would tell and check each element property twice. A type recursive through a
  container must not have `Structural X` as its `Property`, see the recursion entry above. Part of
  <https://github.com/turion/shroom/issues/18>.
* New: `Structural a` (with `structuralHolds`, `describeStructural`) and `PropertyOf`;
  `Describable`/`Surveyable` instances for `Text`, `Int`, `Integer`, `Natural`, `Double`, `Float`,
  `Bool`, `Char`, `UTCTime`, `Day`, `[a]`, `NonEmpty a` and `Maybe a`, so these can be fields of a
  surveyed type; `deriveDescribable` now describes a sum type's constructors.
* New: `Describable` and `Surveyable` instances for `GHC.Generics.Generically a` (forwarding to `a`),
  the superclass instances that `deriving via Generically X instance Promptable X` needs in `shroom`.
* New dependencies: `generics-sop ^>=0.5.1` and `time ^>=1.14`.

## 0.1.0.0

* Initial release. `Data.Shroom.Class` (`Describable`, `Surveyable` and their helpers) and
  `Control.Monad.Prompt.TH` (`deriveDescribable`) were split out of `shroom`, unchanged and under
  the same module names. `shroom` re-exports both, so code importing them through `shroom` needs no
  change.
