# shroom-class

- Nothing in this package may import `Control.Monad.Prompt` or any other module of the `shroom`
  package. This package is the pure half: it has no HTTP client and no effect system, so that a
  consumer such as shikumi can depend on it alone. The package boundary enforces it: `shroom-class`
  does not depend on `shroom`, so such an import does not compile. In docs, write `shroom` names as
  plain code naming the package, not as links, since haddock cannot resolve them from here.
- `Data.Shroom.Class.TH` reads Haddock comments with `getDoc`, so a module that splices
  `deriveDescribable` must be compiled with `-haddock`. This package sets it in its own
  `ghc-options`; a user's package does not get it from depending on this one and must set it
  itself. Do not document it as automatic.
- Docs: write "property", never "invariant". Do not use `\_` in a `@` code block to show a lambda:
  haddock renders it as `_`; write `\\_`.
