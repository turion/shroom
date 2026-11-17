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
- The golden files of `shroom-class` (`shroom-class/test/golden/`) are regenerated with `cabal test shroom-class-test --test-options=--accept`; review the diff afterwards.
- Before every push, run `jj fix` (fourmolu, cabal-gild, `hlint --refactor`; CI checks the first two and hlint) and then the build and test gates again. Build afterwards, because a tool can change meaning: fourmolu once rewrote `OverloadedRecordDot` access `a.b` into `a . b`, and an hlint refactor can introduce a partial function such as `head`, which `-Werror` rejects.

# Documentation standard

- Every PR re-reads the generated haddocks of each file it changes. Build them as CI does: `cabal haddock all -f-anthropic -f-ollama --haddock-hyperlink-source --haddock-quickjump --haddock-html-location='https://hackage.haskell.org/package/$pkg-$version/docs'`. The HTML of each package lands in `dist-newstyle/build/*/*/<package>-<version>/doc/html/<package>/`; open its `index.html` and read the pages of the modules the PR touched. A haddock run passes with unresolved links, so only reading the page shows that a link resolves and that the markup renders as meant.
- A Haddock or code comment describes what the code does, never how it came about. No plan, arc or todo references, no issue numbers, no "as before", "used to", "old behaviour" or "no longer". A real limitation stays, stated plainly. History goes in the CHANGELOG.
- A fact that a future agent needs but a user does not goes in the package's own `AGENTS.md`, imported by a `CLAUDE.md` beside it that holds `@AGENTS.md`.
- The maintainer's documentation standard is the Hackage documentation of rhine, changeset and essence-of-live-coding.

# Key dependency patterns

## openapi3 (`^>=3.2`) — schema generation
- `declareSchemaRef` is in `Data.OpenApi`; `runDeclare` is in `Data.OpenApi.Declare`
- `toJSON (toSchema prx)` alone is NOT enough — omits sub-schemas; use `schemaWithDefs` instead
- `normalizeSchemaForStructuredOutput` (renamed from `fixSchemaForAnthropic` — it's no longer provider-specific): adds `additionalProperties: false`, rewrites `$ref` paths, folds `minimum`/`maximum` into the field's `description` (removed from the wire, not silently dropped). Leaves string `format` alone — Anthropic enforces it; a local model behind llama.cpp enforces only some formats (`uuid`/`date`/`time`/`date-time`, not `email`/`uri`), per `shroom/AGENTS.md`

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

# CI's Ollama integration job

- The job `cabal test`s `shroom-test-integration-ollama` against the maintainer's own Ollama server, reached through an SSH tunnel (`ssh -L 11434:127.0.0.1:11434 ci-ollama@$OLLAMA_SERVER_HOST`); the private key is the secret `OLLAMA_SSH_KEY`, the host the variable `OLLAMA_SERVER_HOST`.
- The server side is `nix/ollama-shroom.nix` (`services.ollama.shroom`, `auth.mode = "ssh-tunnel"`): sshd restricts the key to that one port forward (`permitopen`, `command=""`), so a leaked key can reach Ollama and nothing else. The job pins the host key with `ssh-keyscan` instead of disabling host-key checking.
- `OLLAMA_MODEL` in `ci.yml` must equal the module's `model` on the server (`llama3.2:1b`) and the fallback in `shroom-baikai/test/Integration/Ollama.hs`; a mismatch fails with `not_found_error` and tests nothing about shroom. Keep the three in step by hand.
- The server has 2 cores and no GPU, so the job skips the `conferenceChain` cases (they test model capability, not plumbing) and caps each case at 10 minutes.

# Shell behaviour

- Don't create `/tmp` files, just make edits in the project
- Don't use python. Use jq for json analysis
- An anthropic api key is available in the env, no need to read it separately

# Documentation

I don't like the word "invariant", use "property" instead
