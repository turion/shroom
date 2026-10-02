# Language

Please always speak like a 1970s British working class person!

# Docs

- No local dependency doc lookups (not in /nix/store, ~/.cabal)
- Lookup docs on Hoogle or Hackage
- Use Hoogle skill
- Instead of looking locally, just lookup docs online and trust them for starters
- If unclear, ask me for assistance on how to find docs!


# Coding

- Look at HLS errors
- For small experiments, use cabal repl
- Assume standard Haskell boot/base packages are available (filepath, directory, etc.) without checking locally. Just add them to cabal deps and proceed.
- After each finished plan, run `cabal build all --enable-tests`. It should finish without warnings.
- After each finished plan, run `cabal test` and summarize the test results.

# Key dependency patterns

## openapi3 (`^>=3.2`) — schema generation
- `declareSchemaRef` is in `Data.OpenApi`; `runDeclare` is in `Data.OpenApi.Declare`
- `toJSON (toSchema prx)` alone is NOT enough — omits sub-schemas; use `schemaWithDefs` instead
- `normalizeSchemaForStructuredOutput` (renamed from `fixSchemaForAnthropic` — it's no longer provider-specific): adds `additionalProperties: false`, rewrites `$ref` paths, folds `minimum`/`maximum` into the field's `description` (removed from the wire, not silently dropped). Leaves string `format` alone — Anthropic enforces it; a local model behind llama.cpp enforces only some formats (`uuid`/`date`/`time`/`date-time`, not `email`/`uri`), per `Control.Monad.Prompt.Schema`'s Haddock

## sop-core (`^>=0.5`) — heterogeneous tool lists
- Tools registered as `NP ToolHandler '[Tool1, Tool2, ...]`; `hcmap`/`hcollapse`/`K` for traversal

# Shell behaviour

- Don't create `/tmp` files, just make edits in the project
- Don't use python. Use jq for json analysis
- An anthropic api key is available in the env, no need to read it separately

# Documentation

I don't like the word "invariant", use "property" instead
