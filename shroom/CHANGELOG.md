# Revision history for shroom

## Unreleased

**shroom becomes a layer.** shroom stops owning any LLM transport. A new package,
`shroom-baikai`, provides the adapters instead; `shroom` itself now depends on no LLM client at
all. The program vocabulary moved onto `effectful`, and a program's tools are now part of its
type.

* **Package split**: `shroom`'s own hand-rolled Anthropic and Ollama backends are gone (along with
  the `claude` and `ollama-haskell` dependencies that came with them). `shroom-baikai` replaces
  both, interpreting the `baikai` family of libraries' effect into shroom's own `Backend`
  interface: `claudeBackend` for the real Claude API, `anthropicCompatBackend` for any other
  Anthropic-compatible host, `localOllamaBackend` for a local Ollama server, and
  `openAICompatBackend` for any other OpenAI-compatible host. `FileMock` stays in `shroom` itself
  as the no-key, no-network way to see a rendered prompt.
* **`PromptT` and `PromptResultT` are deleted, not deprecated** — along with `runChatWithTools`.
  The hand-rolled operational monad, its interpreter and the `ReaderT`/`ExceptT` stack underneath
  are replaced by ordinary `effectful` effects in `Control.Monad.Prompt.Effect`: context is
  `State`, scoping is `local`-style, failure is `Error Text`, and a chain runs end to end via
  `runPromptResultEff` against a `Backend` value.
  - `Promptable`'s single method changed from `promptDescription :: Proxy a -> Text` to
    `prompt :: (Prompt :> es) => Eff es a` — an instance now writes a program, not a text renderer.
  - **Behaviour change: `<*>` is no longer concurrent.** Two independent `prompt` calls combined
    with `<*>` now run sequentially, like any other `Applicative`. The old `PromptT` ran `<*>`
    through `unliftio`'s `concurrently` implicitly; nothing in `effectful` does that on its own,
    and hiding concurrency inside an operator was a surprise, not a feature. Use `promptPar` (two
    programs) or `promptsParallel` (a list) to run concurrently explicitly.
  - `Alternative` / `MonadPlus` are gone with it — `Eff` has no such instance. `orElse` replaces
    `<|>`: try the left program, and on failure restore context to before the attempt and run the
    right one, the same fallback behaviour `<|>` used to give.
* **Tools as effects**: a program now names the tools it may call in its own effect row —
  `(Tool MySearch :> es, Prompt :> es) => Eff es a` — and a program reaching for a tool it was not
  given fails to compile. Build a binding with `toolBinding @MySearch`, offer it to `promptTools`,
  and supply the handler as an interpreter, `runTool mySearchHandler`. The older runtime-registry
  path (`NP ToolHandler '[...]`, `makeDispatcher`, `toolDefsRaw`, built on `sop-core`) is still
  there for callers who want a heterogeneous list instead of effect-row membership.
* Two built-in web tools ship in `Control.Monad.Prompt.Tool.Web`: `WebFetch` (HTTP GET, HTML
  stripped, truncated) and `DuckDuckGoSearch`/`WikipediaSearch` (named-entity lookup, falling back
  to Wikipedia's own search).
* **`ContextItem`**: gained `ToolCallMessage [ToolCall]` and `ToolResultMessage [ToolResult]`
  constructors, so a tool exchange round-trips through the same history type as everything else.
  `addContextItem` / `withContextItem` remain for direct use; `context` is still the shorthand.
* **`normalizeSchemaForStructuredOutput`** (renamed from `fixSchemaForAnthropic` — it isn't
  provider-specific): adds `additionalProperties: false`, rewrites `$ref` paths, and folds
  `minimum`/`maximum` into the field's description instead of dropping them. String `format` is
  left on the wire: Anthropic enforces it, and a local model behind llama.cpp enforces some tags
  (`uuid`/`date`/`time`/`date-time`) but not others (`email`/`uri`).
* **`inlineSchema` is wired into `schemaWithDefs`, not dead code any more**: a live Ollama resolves
  a single `$ref` hop correctly but silently stops enforcing the schema on a second hop — exactly
  what any nested-record type produces — so every schema `shroom` sends is now fully flattened.
* **`debugLog`**: `PromptConfig` still has `debugLog :: Maybe (Text -> IO ())`, logging attempt
  number, context, response and the OK/PARSE FAIL/VALIDATION FAIL/ERROR outcome of each call,
  unchanged in shape from before this arc.
* **Dependency changes**: `shroom` drops `claude`, `ollama-haskell`, `mtl` and `transformers`, and
  adds `effectful ^>=2.7`. `shroom-baikai` is new and depends on `baikai`, `baikai-claude` and
  `baikai-openai` (`>=0.7 && <0.8`) plus `baikai-effectful` (`>=0.4 && <0.5`).

## 0.1.0.0 -- 2026-05-28

* First version. Released on an unsuspecting world.
