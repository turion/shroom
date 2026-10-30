# Revision history for shroom-class

## Unreleased

* **New: a property as a value, `PropertyCheck a`, in the new module `Data.Shroom.Class.PropertyCheck`
  (re-exported by `Data.Shroom.Class`).** `PropertyCheck {description :: Maybe Text, holds :: a ->
  Bool}` is one property: what the model is told (`Nothing`: left out of the prompt's property list)
  and the check. The module has `NoFieldSelectors`, so the field `description` does not clash with
  the `description` function; read a field with `OverloadedRecordDot` (`c.description`, `c.holds`).
  Record construction next to the `description` function needs `GHC2024` or
  `DisambiguateRecordFields`; under `Haskell2010` or `GHC2021` build it positionally,
  `PropertyCheck (Just "…") check`. Also new: `typeProperties :: Surveyable a => [PropertyCheck a]`,
  one entry per element of `universe @(Property a)` with `describeProperties`'s description and the
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
* **Breaking: `Surveyable`'s `Property` now defaults to `Structural a`, not `Void`.** A bare
  `instance Surveyable X` used to have no properties; it now has the properties of `X`'s fields —
  each field type's own `Property` — described in the prompt as `Field name: …` and checked on the
  parsed value, by default `propertyHolds`/`describeProperties` (`structuralHolds`,
  `describeStructural`). The default needs `SOP.Generic` and `SOP.HasDatatypeInfo` for `X` and a
  `Surveyable` instance for every field type, so an existing bare instance stops compiling until you
  take one of two ways out:
  - **Derive the `generics-sop` classes**, next to your other `deriving anyclass`:
    `deriving anyclass (…, SOP.Generic, SOP.HasDatatypeInfo)` (with
    `import Generics.SOP qualified as SOP`, and `generics-sop` in your `build-depends`). The
    instance then surveys the fields; if none of them has properties, nothing in the prompt changes.
  - **Opt out** with one line and no methods: `instance Surveyable X where type Property X = Void`.
    Use this for a field of a type you do not own, which has no `Describable`/`Surveyable`
    instance (a bare instance fails with an `All2 Surveyable` error), or when you do not want the
    `generics-sop` instances. Or give that field type, or a newtype around it, its own instances.
* **Breaking: a directly or mutually recursive type with a bare `instance Surveyable X` hangs.**
  With the structural default, `universe @(Structural X)` never ends for a type with a field of its
  own type that is not under a list, `NonEmpty` or `Maybe` (`data Expr = Lit Int | Neg Expr`), or
  that reaches itself through another type's bare instance. `description` and shroom's retry loop
  (which enumerate the properties) then run forever, in `shroom` before
  any request is sent and on every backend; under the old `Void` default such a type worked. Opt out
  on the recursive type with `instance Surveyable X where type Property X = Void`, or give it a
  custom `Property` that does not contain `Structural X`. No attempt is made to make `Universe`
  terminate.
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
  No instance for ‘PropertyDefault XProperty X’
    arising from a use of ‘Data.Shroom.Class.$dmdescribeProperties’
  ```

  (`$dmpropertyHolds` when `propertyHolds` is the one missing). Write the method; for an informal
  property, `propertyHolds _ _ = True` or `describeProperties _ _ = Nothing` is the old default.
  `PropertyDefault` is the dispatch class behind those defaults, exported for its name's sake.
* **A custom `Property` replaces the structural default entirely.** Adding one property of your own
  to a type silently drops its fields' properties from the prompt and from validation. To keep both,
  combine them: `type Property X = Either XProperty (Structural X)`, with `structuralHolds` and
  `describeStructural` on the `Right` side; the `Surveyable` Haddock has the four equations.
* **Container fields contribute no properties.** `[a]`, `NonEmpty a` and `Maybe a` have
  `Property = Void` whatever `a` is, so under a bare instance the element properties of a `[Speaker]`
  or `Maybe Name` field stay unchecked, and are not told to the model either, until container
  lifting is derivable (<https://github.com/turion/shroom/issues/18>). Until then lift them with
  `everyHolds`/`describeEvery` in a custom `Property` combined with `Structural X`, as the `Speakers`
  example in `everyHolds`'s Haddock does.
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
