# Revision history for shroom-class

## Unreleased

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
  that reaches itself through another type's bare instance. `description` and the checks that
  enumerate the properties (`violatedProperties` in `shroom`) then run forever, in `shroom` before
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
