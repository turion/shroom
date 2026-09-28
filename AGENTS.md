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

## claude (`^>=1.4.0`) — Anthropic API client
- Modules: `Claude.V1`, `Claude.V1.Messages`
- Request: `_CreateMessage { model, messages :: Vector Message, system :: Maybe SystemPrompt, max_tokens, tools, output_config }`
- System prompt: `systemText :: Text -> SystemPrompt`
- Structured output: `output_config = Just (jsonSchemaConfig schema)`
- Tool use: register via `tools = Just (V.fromList toolDefs)`, detect `stop_reason = Just Tool_Use`, extract `ContentBlock_Tool_Use {tool_use_id, name, input}`, reply with `Content_Tool_Result {tool_use_id, content, is_error}`
- Default model: `"claude-haiku-4-5-20251001"` (NOT `claude-3-5-haiku-20241022` — returns 404)

## ollama-haskell (`^>=0.2`) — Ollama API client
- Modules: `Data.Ollama.Chat`, `Data.Ollama.Common.Config/Error/SchemaBuilder/Types`
- Request: `defaultChatOps { modelName, messages :: NonEmpty Message, format = Just fmt, stream = Nothing }`
- Messages: single flat list; `systemMessage`/`userMessage`/`assistantMessage` constructors; type desc always appended as final user turn
- Structured output: `Format` supports objects only; scalars/arrays wrapped in `{"result": <val>}` and unwrapped after
- Ollama's *server* (tested at 0.30.6) resolves `$ref` fine on the wire, on both the `format` field and a tool's parameters — but `ollama-haskell`'s own typed `Schema`/`FunctionParameters` have no field for `$ref`/`$defs` at all, and `schemaWithDefs` always `$ref`s its own root, so `inlineSchema` must still run first at both call sites in `Ollama.hs` — not a wire-protocol workaround, a client-library one
- Tool support: not implemented (stub)

## openapi3 (`^>=3.2`) — schema generation
- `declareSchemaRef` is in `Data.OpenApi`; `runDeclare` is in `Data.OpenApi.Declare`
- `toJSON (toSchema prx)` alone is NOT enough — omits sub-schemas; use `schemaWithDefs` instead
- `normalizeSchemaForStructuredOutput` (renamed from `fixSchemaForAnthropic` — it's no longer provider-specific): adds `additionalProperties: false`, rewrites `$ref` paths, folds `minimum`/`maximum` into the field's `description` (removed from the wire, not silently dropped). Leaves string `format` alone — Anthropic accepts it, and it's an annotation, not an enforced constraint

## sop-core (`^>=0.5`) — heterogeneous tool lists
- Tools registered as `NP ToolHandler '[Tool1, Tool2, ...]`; `hcmap`/`hcollapse`/`K` for traversal

# Shell behaviour

- Don't create `/tmp` files, just make edits in the project
- Don't use python. Use jq for json analysis
- An anthropic api key is available in the env, no need to read it separately

# Documentation

I don't like the word "invariant", use "property" instead
