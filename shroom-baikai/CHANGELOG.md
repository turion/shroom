# Revision history for shroom-baikai

## Unreleased

* **Builds against the baikai 0.8 line**: `baikai` 0.8.0.0, `baikai-claude` 0.7.1.1,
  `baikai-openai` 0.7.1.1 and `baikai-effectful` 0.4.0.3. The `baikai` bound widens to
  `>=0.7.0 && <0.9`; the other three already admitted those releases. baikai 0.8's one breaking
  change, the new `longCacheWriteCost` field of `Baikai.Model.InputPriceTier`, touches nothing
  shroom-baikai uses, so the same code compiles against 0.7 and 0.8. The Nix flake now pins the
  0.8 line.
* The `text` lower bound drops from 2.1.3 to 2.1.2, matching `shroom` and `shroom-class`, so the
  `text` that ships with GHC 9.12.2 is accepted.
