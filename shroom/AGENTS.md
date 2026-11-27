# shroom

## Schema wire facts (`Control.Monad.Prompt.Schema`)

These are observations about the hosts' structured-output handling that explain what `schemaWithDefs`,
`normalizeSchemaForStructuredOutput` and `inlineSchema` do. Do not document them in Haddocks as history;
the Haddocks say what the functions do.

- **`inlineSchema` is load-bearing.** A live Ollama (`0.30.6`, `llama3.2:1b`) resolves a single `$ref` hop
  correctly, on both the response `format` field and a tool's parameter schema, including on a deliberately
  unguessable field name. It silently stops enforcing the schema (an empty array or invented field names, no
  error) as soon as it must follow a second hop, a `$ref` found while resolving another `$ref`. Any
  nested-record type produces that: an array of records, or a record containing a record. A flat record is
  one hop from the root, so it worked without inlining. `schemaWithDefs` therefore flattens every `$ref`.
- **Inlining is capped at 20 levels.** A self-referential type has no finite fully inlined form; past the
  cap the rest is left undescended instead of looping.
- **`oneOf` becomes `anyOf`.** `openapi3` renders a sum type with a non-nullary constructor as `oneOf`;
  Anthropic's structured outputs list `anyOf` and `allOf` as supported, not `oneOf`. The alternatives
  `openapi3` generates are told apart by their tag, so "at least one" accepts what "exactly one" did.
- **Only supported string formats are sent.** Anthropic's documentation lists `format` among the schema
  features it enforces (`date-time`, `date`, `time`, `duration`, `email`, `hostname`, `uri`, `ipv4`, `ipv6`,
  `uuid`), and the live API answers any other format with a `400` (measured 2026-11-27: `For 'string' type,
  format 'yyyy-mm-ddThh:MM:ssZ' is not supported`). That string is `openapi3`'s format for `UTCTime`, so
  `normalizeSchemaForStructuredOutput` renames it to `date-time` and folds every other unsupported format,
  such as `TimeOfDay`'s `hh:MM:ss` or `LocalTime`'s, into the description. CI runs no suite against the
  real Claude API, so only a local `cabal test shroom-test-integration-claude` catches such a `400`. A
  local model behind llama.cpp is uneven: its grammar converter compiles `uuid`, `date`, `time` and
  `date-time` into grammar rules and leaves `email` and `uri` unconstrained. One live test with `format:
  email` got back `"thompsons sophia@outlook.com "`, with a stray space inside. Which host it ran against
  was not recorded; the failure's shape matches llama.cpp's converter, and a direct call to
  `api.anthropic.com` had returned a `401`, so no successful Anthropic call backs this up.
- **Base types' schemas, measured against Claude on 2026-11-27** (`claude-haiku-4-5`, each schema as one field
  of an object). `Char`'s `minLength`/`maxLength` and `Double`'s `format: double` are accepted. `Natural`
  carries `openapi3`'s OpenAPI 3.0 boolean `exclusiveMinimum: false`, which Claude refused with `False is not
  of type 'number'`; `normalizeSchemaForStructuredOutput` therefore folds a boolean exclusive bound into the
  description with `minimum`/`maximum`, and leaves a numeric one alone. The `Promptable` instances for `[a]`,
  `NonEmpty a`, `Set a` and `Maybe a` pass the element's schema through, so they share their element's fate.
  The `Promptable` Haddocks stay provider-neutral and do not say this.
- **Open question:** whether a llama.cpp-backed grammar enforces the integer bounds numerically; they are
  folded into the field's `description` and removed from the wire. The `Surveyable` properties exist to
  check in code what a host does not enforce.

## `Generically` in `Control.Monad.Prompt.Promptable`

- **`ToSchema (Generically a)` is an orphan, on purpose** (`{-# OPTIONS_GHC -Wno-orphans #-}`, which covers the
  whole module). It is missing from `openapi3`, which is where it belongs; it forwards to `a`'s schema and
  exists to satisfy `Promptable`'s superclass. It is the module's only orphan. `Promptable (Generically a)` sits
  beside the class, because `Control.Monad.Prompt.Effect` does not import `Control.Monad.Prompt.Promptable`, so
  the strategy can call the class's own `prompt` on every field and `improve` from `Effect`. `Describable` and
  `Surveyable` for `Generically` are not orphans: they are in `Data.Shroom.Class`. The context of the
  `Promptable` instance is mostly about `a`; `ToJSON (Generically a)` and `FromJSON (Generically a)` are
  `aeson`'s generic ones, needed by the superclasses.
- **`promptSingle` and `improve` make single requests on purpose,** so the strategy may call `improve` without recursing
  into itself; only the class method `prompt` runs a `deriving via Generically` strategy.

## `Control.Monad.Prompt.Tool`

- **`Toolable` is deliberately not a subclass of `Promptable`.** A tool's input type is described and given a
  schema the way a `Promptable` type is, but it is never the target of a `prompt` call. Requiring `Promptable`
  would add a method nobody uses and make `Tool` depend on `Control.Monad.Prompt.Effect`, which it does not
  otherwise need. The superclasses are `Surveyable`, `ToSchema`, `FromJSON` and `Typeable`.
- **`dispatchBindings` is a run-time scan by name; only dispatch is.** Whether a program may call or offer a
  tool is checked at compile time, through `Tool t :> es` on `callTool` and `toolBinding`. Finding the tool the
  model named from a bare string needs run-time reflection over `es`, which `effectful` does not offer, so
  every `ToolBinding` closes over its own tool type and the list is searched by `toolDefName`. Full
  type-level dispatch was not attempted beyond that.
- **`Control.Monad.Prompt.Tool.Web`'s three tool types are prompt text.** Their own and their fields' Haddocks
  are read by `deriveDescribable` at compile time into `describeType`, which the model is sent; so are their
  `examples` and `toolDescription`. Editing one changes what the model sees. Known mismatch: the Haddock of
  `DuckDuckGoSearch` says multi-word queries return no result, while its `examples` are multi-word queries.
  It is left as it is so the model's prompt does not change.
- **`cabal.project` lists the `Rep_*` symbols of `Control.Monad.Prompt.Tool.Web` by hand.** Haddock warns about
  the link to the representation type GHC derives for each tool type's `Generic` instance, since it has no page.
  The `package shroom` stanza silences it with one `--ignore-link-symbol` per tool type
  (`Rep_WebFetch`, `Rep_DuckDuckGoSearch`, `Rep_WikipediaSearch`). A new `Generic` tool type in that module needs
  a fourth one, or the CI doc build warns.
- **`trySynchronous` catches synchronous exceptions only.** A failed request (connection refused, bad status)
  becomes a `Left` tool result; an asynchronous exception (`System.Timeout.timeout`'s `Timeout`,
  `ThreadKilled`, an `async` cancellation) is the caller's control flow and propagates, so a `timeout` around
  a hung request returns `Nothing` instead of handing the model a timeout result and carrying on.

## `Control.Monad.Prompt.Effect`

- **`improve`'s wire form is not in its Haddock.** The request shows the value as JSON in a scoped
  `UserMessage`, which the backend sees and the context left behind does not; only the raw reply is kept, as
  an `AssistantMessage`. The model is asked for one object, `{"verdict": "fine" | "improved", "value": <a>}`.
  Both keys are `required`, because strict structured-output modes reject an object whose properties are not
  all required; a `fine` verdict ignores `value`, and its parser accepts a reply without one for backends that
  do not enforce `required`. The root is built by hand, not derived, so that no `oneOf` sits at the root.
- **`Prompt` is exported abstract on purpose.** Its three constructors (`RequestPrompt`, `RequestPromptTools`,
  `RequestImprove`) are not public API, so they carry no Haddocks of their own; `runPrompt` is the interpreter.
