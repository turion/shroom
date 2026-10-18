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
    `prompt :: (Prompt :> es, State [ContextItem] :> es) => Eff es a` — an instance now writes a
    program, not a text renderer (see the breaking note on the `State` constraint below).
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
  path (`makeDispatcher`, `ToolLoopOps`, `genericToolLoop`) had no call sites anywhere in the repo
  and has been deleted. `toolDefsRaw` survives — it extracts `ToolDef` metadata from an
  `NP ToolHandler '[...]` and is still exercised directly — so `sop-core` remains a dependency.
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
* **`shroom-class` split out**: `Data.Shroom.Class` (`Describable`, `Surveyable` and their helpers)
  and `Control.Monad.Prompt.TH` (`deriveDescribable`) now live in the new `shroom-class` package,
  under the same module names, and `shroom` re-exports both. Imports and `build-depends` that go
  through `shroom` are unchanged. A `cabal.project` that lists shroom's packages by path, or a
  `source-repository-package` with `subdir:`, must now also list `shroom-class`. `shroom`'s `text`
  lower bound is now 2.1.2.
* **Breaking: a bare `instance Surveyable X` now surveys `X`'s fields.** `shroom` re-exports
  `Data.Shroom.Class`, and `Surveyable`'s `Property` now defaults to `Structural a`, not `Void`: a
  bare instance needs `SOP.Generic` and `SOP.HasDatatypeInfo` for `X` (and `generics-sop` in your
  `build-depends`) or the one-line opt-out `type Property X = Void`, and a custom `Property` must now
  write both `propertyHolds` and `describeProperties`. `shroom-class`'s changelog has the migration
  note, the compile error to search for, and what container fields do (nothing yet).
* **Breaking: a directly or mutually recursive type with a bare `instance Surveyable X` hangs**
  before any request is sent, on every backend, because the structural default enumerates its
  properties forever (e.g. `data Expr = Lit Int | Neg Expr`). Opt out with
  `type Property X = Void` or a custom `Property`; `shroom-class`'s changelog has the details. (A
  recursive schema is in any case rejected by Claude.)
* **Breaking: the new base-type `Promptable` instances can overlap yours.** `Promptable` now has
  instances for `Text`, `Int`, `Integer`, `Natural`, `Double`, `Float`, `Bool`, `Char`, `UTCTime`,
  `Day`, `[a]`, `NonEmpty a` and `Maybe a` (listed under "New" below), and `shroom-class` gives the
  same types `Describable` and `Surveyable` instances. A downstream package that already defines one
  of them, say `instance Promptable Text` or `instance Describable [Foo]`, now gets a
  duplicate-instance or overlapping-instances error; delete its instance, or wrap the type in a
  `newtype` of your own.
* **Breaking: `Promptable`'s method `prompt` now also needs `State [ContextItem] :> es`**, so an
  override can scope context with `withContext` around the default request. This breaks more than
  overrides: any polymorphic user function `(Prompt :> es) => … ` that calls the class method
  `prompt` (not the flat `Control.Monad.Prompt.Effect.promptSingle`, formerly `prompt`, which is
  unaffected) must now also require
  `State [ContextItem] :> es` (`State` from `Effectful.State.Static.Local`, `ContextItem` from
  `Control.Monad.Prompt.Backend`). A chain that already calls `context` has that constraint
  anyway, and `runPromptResultEff` supplies it, so a concrete program is unaffected.
* **Breaking: `Control.Monad.Prompt.Effect.prompt` is now `promptSingle`, and `promptWith` is now
  `promptSingleWith`** (#27). Two functions named `prompt` were in play, and the flat one in
  `Effect` silently ignored a type's `Promptable` strategy (for instance `deriving via
  Generically`) while the class method honoured it, so importing both unqualified was ambiguous,
  and picking the wrong one compiled and ran but skipped the strategy. The flat request keeps its
  behaviour under its new name and never consults `Promptable`; no deprecated alias is left
  behind, since an alias named `prompt` would keep the ambiguity. `Promptable`'s default is
  now `prompt = Effect.promptSingle`. Migrate: call the class method `prompt` (from
  `Control.Monad.Prompt`) to honour a type's strategy, or rename a flat call to `promptSingle` or
  `promptSingleWith`. `withContext t (prompt @a)` is the strategy-aware equivalent of
  `promptSingleWith t`.
* **New: base-type `Promptable` instances.** `Text`, `Int`, `Integer`, `Int64`, `Word`, `Natural`, `Double`, `Float`,
  `Bool`, `Char`, `UTCTime`, `Day`, `[a]`, `NonEmpty a`, `Set a` (needs `Ord a`, for `FromJSON`) and
  `Maybe a` are now `Promptable` with the
  default flat strategy, matching the `Surveyable` instances `Data.Shroom.Class` already gives them,
  so a bare value or a field of one of them can be prompted. Caveat: `openapi3` renders `Char`
  (`minLength`/`maxLength`), `Natural` (`exclusiveMinimum`) and, unconfirmed, `Double`/`Float` (a
  numeric `format`) with keywords Anthropic's structured outputs reject with a 400 and
  `normalizeSchemaForStructuredOutput` does not yet strip, so these may fail against Claude until it
  does.
* **New: `improve :: a -> Eff es (Maybe a)`** in `Control.Monad.Prompt.Effect`, a fourth `Prompt`
  request (`RequestImprove`) beside `promptSingle`/`promptSingleWith`/`promptTools`. It shows the model an
  existing value as JSON and gets back one verdict object, `{"verdict": "fine" | "improved",
  "value": <a>}`: `Nothing` means the value is valid and the model is content with it, `Just v`
  is a corrected whole value. Both `verdict` and `value` are `required` in the wire schema (OpenAI's
  strict `json_schema` mode rejects a schema with optional properties): the model returns the
  value unchanged with `fine`, which the parser ignores, and a `fine` reply without `value` is
  still accepted. An `improved` value that fails to parse or violates a property is
  re-prompted like any request, and a `fine` verdict about a value that itself violates a property
  is retried the same way, so `Nothing` is never returned for an invalid value. `improve` is flat:
  it never calls the type's own `Promptable.prompt`. Informal properties, which only the model can
  judge, are still #26.
* **New: `deriving via Generically X instance Promptable X`** prompts a single-constructor record
  field by field (#21). `Control.Monad.Prompt.Generic` (re-exported by `Control.Monad.Prompt`) gives
  `GHC.Generics.Generically a` a `Promptable` instance that, inside one snapshot of the context,
  asks the model for a short `Cue` (a steer every field follows), then prompts each field in order
  through **its own type's** `Promptable.prompt`, told the parent's description, the cue and the
  field's name, each later field seeing the earlier ones; assembles the draft with `generics-sop`;
  runs `improve` on it, keeping the draft on a `fine` verdict; and finally restores the context and
  appends the value's JSON as one `AssistantMessage`, as after a flat `promptSingle`. A record with `n`
  fields costs `n + 2` requests, plus retries and the requests of any generic field.
  Each of those requests now carries the parent's full reachable-type section in its
  context (`promptGeneric` puts `description (Proxy @a)` there once, ahead of the
  first), so the prompt grows with the type tree. `X` needs
  `SOP.Generic` and `SOP.HasDatatypeInfo` (so `generics-sop` in your `build-depends`) and a
  `Promptable` instance for every field type. It takes effect through the class method
  `Promptable.prompt`; `Control.Monad.Prompt.Effect.promptSingle` stays flat.
  `Describable`/`Surveyable (Generically a)` are in
  `Data.Shroom.Class`; `shroom` carries two orphan instances, `ToSchema (Generically a)` (which
  `openapi3` lacks) and `Promptable (Generically a)` itself, so import `Control.Monad.Prompt` or
  `Control.Monad.Prompt.Generic` for them to apply.
* **`deriving via Generically X` also builds sum types** (#22): a type with several constructors is
  prompted by first asking the model which constructor to build, through the new
  `ConstructorChoice X` (a string enum of the constructor names, described with the constructors'
  Haddock; its reply stays in the context like the cue's), then running the product strategy on that
  constructor's fields, with the chosen constructor named in the cue and field messages, then
  `improve`. A constructor with no fields is assembled at once, without cue or field requests
  (`improve` still runs), and a single-constructor type skips the choice, so a product costs what
  it did. A sum with `k` fields in its chosen constructor costs `k + 3` requests (choice, cue,
  fields, improve), a nullary one 2.
* **Wire-schema change: `normalizeSchemaForStructuredOutput` rewrites `oneOf` to `anyOf`.**
  `openapi3` renders every sum type with a non-nullary constructor as `oneOf`, which Anthropic's
  structured outputs do not list as supported (they list `anyOf` and `allOf`), so such a type could
  not be prompted flat either. Every schema `shroom` sends now says `anyOf` where it said `oneOf`,
  including tool parameter schemas (`schemaWithDefs` feeds them too), which is harmless: the
  alternatives `openapi3` generates are told apart by their tag, so "at least one" and "exactly
  one" accept the same values.

## 0.1.0.0 -- 2026-05-28

* First version. Released on an unsuspecting world.
