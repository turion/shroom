# Revision history for shroom-class

## 0.1.0.0

* Initial release. `Data.Shroom.Class` (`Describable`, `Surveyable` and their helpers) and
  `Control.Monad.Prompt.TH` (`deriveDescribable`) were split out of `shroom`, unchanged and under
  the same module names. `shroom` re-exports both, so code importing them through `shroom` needs no
  change.
