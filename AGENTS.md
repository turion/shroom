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

## sop-core (`^>=0.5`) — tool metadata extraction
- `toolDefsRaw :: NP ToolHandler '[Tool1, Tool2, ...] -> [ToolDef]` walks a heterogeneous handler list to build the `ToolDef`s a backend registers with the LLM API; `hcmap`/`hcollapse`/`K` for traversal
- `shroom/test-utils/WebToolReport.hs` uses `sop-core` independently of `toolDefsRaw`, with its own `hcpure`/`hcollapse` traversal over the built-in tool list

## baikai (`baikai`/`baikai-claude`/`baikai-openai` `>=0.7 && <0.8`, `baikai-effectful` `>=0.4 && <0.5`) — transport for `shroom-baikai`
- Four front-door constructors in `Control.Monad.Prompt.Baikai`: `claudeBackend apiKey`;
  `anthropicCompatBackend baseUrl modelId apiKey` (key is a plain `Text`, not `Maybe`);
  `localOllamaBackend modelId` (reads `OLLAMA_HOST` itself, defaults to
  `http://127.0.0.1:11434`); `openAICompatBackend baseUrl modelId mApiKey` (`Maybe Text` key —
  `Nothing` for a host that checks no credential at all, e.g. a bare local Ollama or llama.cpp)
- `baseUrl` takes no trailing `/v1` — `Baikai.Http.canonicalBaseUrl` strips one and the transport
  appends its own; giving one composes to `/v1/v1/...`
- No native Ollama provider — `localOllamaBackend` goes through `baikai-openai`'s OpenAI-compatible
  Chat Completions client instead, since Ollama's own server speaks that wire format
- `baikaiBackend :: Model -> Options -> Backend m` makes one `complete` call per `runBackendChat`;
  wrapped in `try @SomeException` so nothing escapes as an IO exception, per `Backend`'s contract
- `Baikai.Tool.mkTool`'s `parameters` field is an opaque `Value` — `schemaWithDefs`'s already-flat
  schema passes straight through, no separate inlining needed on this side
- `baikai-openai` sends the token cap as `max_completion_tokens` by default and Ollama ignores that
  field (measured: `max_completion_tokens=16` → 991 tokens, `max_tokens=16` → 16). `openAICompatBackend`
  therefore sets `Model.compat = CompatOpenAICompletions … {maxTokensField = MaxTokensField}` (kept
  off `api.openai.com`, whose newer models reject `max_tokens`); the offline suite asserts the wire body

# Shell behaviour

- Don't create `/tmp` files, just make edits in the project
- Don't use python. Use jq for json analysis
- An anthropic api key is available in the env, no need to read it separately

# Documentation

I don't like the word "invariant", use "property" instead
