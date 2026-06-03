# shroom 🍄

> *Type-safe AI hallucinations for Haskell.*

LLMs hallucinate.
But with **shroom**, at least your hallucinations will be well-typed and guaranteed to fulfill your specs.
If the model makes something up, it'll be a properly structured something.

## What is it?

**shroom** is a Haskell library for structured LLM prompting. You write a
monadic program using `context` and `prompt`, pick a backend
(Anthropic Claude or a local Ollama model), and get back a typed Haskell value
— or a `Text` error explaining what went wrong.

```haskell
-- | A user with a name and an email address.
data User = User { userName :: Text, userEmail :: Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema, Show)

$(deriveDescribable ''User)

data UserProperty = UserEmailHasAtSign
  deriving (Bounded, Enum, Universe, Eq, Ord, Show)

instance Surveyable User where
  type Property User = UserProperty
  describeProperties _ UserEmailHasAtSign = Just "The email address must contain an '@' character."
  propertyHolds user UserEmailHasAtSign = T.elem '@' (userEmail user)

deriving via DescriptionPrompt User instance Promptable User

main = do
  user <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig $ do
    context "Give me a user starting with A"
    prompt @User
  print user -- Might print User { userName = "Alice", userEmail = "alice@example.org" } or something else entirely
```
The model hallucinates something. `shroom` ensures it's a type-checking `User`.

## Backends

| Backend | Config type | Module | Notes |
|---|---|---|---|
| Anthropic Claude | `AnthropicConfig` | `Control.Monad.Prompt.Anthropic` | Requires `ANTHROPIC_API_KEY`; uses structured JSON output |
| Ollama (local) | `OllamaBackendConfig` | `Control.Monad.Prompt.Ollama` | Free, offline, slower; defaults to `llama3.2:3b` |
| File mock | `FileMockConfig` | `Control.Monad.Prompt.FileMock` | Reads canned JSON responses from files; useful for dev/testing |

## The typeclass hierarchy

shroom uses three typeclasses that form a hierarchy:

```
Describable          — describeType: one sentence (auto-derived from Haddock)
    └── Surveyable   — Property, propertyHolds, describeProperties, examples (user-filled)
            └── Promptable (+ ToSchema)  — promptDescription: how to render the full prompt
```

- **`Describable`** — attach a one-sentence description to a type.
  Use `$(deriveDescribable ''MyType)` from `Control.Monad.Prompt.TH` to derive
  `describeType` automatically from your Haddock comment.

- **`Surveyable`** — the main user-filled class. Override what you need:
  - `Property` — an enumeration of properties (default: none)
  - `propertyHolds` / `describeProperties` — properties to validate after parsing
  - `examples` — sample values to include in the prompt

- **`Promptable`** — determines how the full prompt fragment is rendered.
  The default (`promptDescription = description`) renders the type description,
  properties, and examples annotated with JSON field types.
  Override `promptDescription` for fully custom prompt text.

## Prompt chaining

`PromptT` is a monad, so you can chain multiple prompts together. Each call to
`prompt` or `promptWith` is a separate LLM request, but they share the same
accumulated context:

```haskell
result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig $ do
  context "The user Alice is an expert programmer:"
  user  <- prompt @User                                         -- first LLM call
  score <- promptWith @Score "Rate this user's awesomeness."   -- second LLM call, still sees "The user Alice is an expert programmer:"
  pure (user, score)
```

## Parallel prompts

Independent prompts can run concurrently. The `Applicative` instance uses
`unliftio`'s `concurrently` under the hood, so both LLM calls are issued at
the same time:

```haskell
-- Two independent prompts run concurrently — plain Applicative:
(user, score) <- (,) <$> prompt @User <*> prompt @Score

-- A list of independent prompts, all in parallel:
talks <- sequenceA
  [ promptWith @Talk ("Speaker: " <> speakerName spk) | spk <- speakers ]
```

`promptPar` and `promptsParallel` are convenience aliases for the above:

```haskell
(user, score) <- promptPar (prompt @User) (prompt @Score)
talks <- promptsParallel [ promptWith @Talk ... | spk <- speakers ]
```

Context added *inside* a parallel branch is local to that branch — it does not
leak to sibling branches or to subsequent steps in the chain.

## Context model

- `context "..."` adds text to the *global* conversation context, visible to
  all subsequent prompts in the chain.
- `promptWith @T "..."` sends a *prompt-local* extra message alongside one
  specific request — it doesn't accumulate into the global context.
- For fine-grained control, `addContextItem` and `withContextItem` accept
  `ContextItem` values (`SystemMessage`, `UserMessage`, or `AssistantMessage`)
  directly, letting you build structured multi-turn conversation history.

## Branching with `Alternative`

`PromptT` implements `Alternative` and `MonadPlus`. Use `<|>` to try one branch
and fall back to another if it fails. Context added in a failing branch does not
leak to the fallback or to subsequent steps:

```haskell
result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig $
  prompt @RichUser <|> fmap toRichUser (prompt @SimpleUser)
```

## Prior art & inspiration

shroom was directly inspired by Gabriella Gonzalez's blog post
[*Prompt chaining reimagined with type inference*](https://haskellforall.com/2025/05/prompt-chaining-reimagined-with-type_2)
(Haskell for All, 2025).

### Grace

The post introduces **Grace**, a research DSL for prompt engineering built around
*bidirectional type inference*: rather than manually declaring JSON schemas, Grace
infers them from how outputs are used downstream in the program. For example:

```grace
numbers.x + numbers.y : Integer
```

The compiler sees the field accesses and arithmetic, and automatically generates the
JSON schema `{ x: Integer, y: Integer }` to send to the model — no schema
declaration needed. Field names can embed human-readable descriptions
(`"The character's personal arc": Text`), sum types model tool selection, and
`import prompt` lets a model generate Grace expressions (recursive prompt chains).

Grace is a proof-of-concept research prototype. It lives on
the [`gabriella/llm`](https://github.com/Gabriella439/grace) branch of the Grace
repository.

### How shroom relates to Grace

shroom takes the same core idea — typed, structured, multi-step LLM prompting in
Haskell — but as a **library** embedded in ordinary Haskell rather than a new DSL.
The tradeoff:

| | Grace | shroom |
|---|---|---|
| Schema source | Inferred from usage | Explicit `ToSchema` instance |
| Sum-type routing | ✓ (inferred) | ✓ (`prompt @SumType`, then `case`) |
| Property validation + retry | — | ✓ (`Surveyable` / `propertyHolds`) |
| Parallel prompts | — | ✓ (`promptPar`, `promptsParallel`) |
| Multiple LLM backends | — | ✓ (Claude, Ollama, FileMock) |
| Code generation | ✓ | — |
| Haddock → prompt description | — | ✓ (TH splice, field docs auto-annotated) |
| Production-ready | proof-of-concept | closer |
| Embedding | own DSL | full Haskell |
| `FromJSON` / schema must agree | n/a | ✓ (not enforced automatically — must be consistent) |

The main thing shroom does *not* have from Grace is automatic schema inference —
you write `ToSchema` and `Describable`/`Surveyable` instances yourself (or derive them). In
exchange you get property validation, automatic retry with feedback, and backends
that actually work today.

### intelli-monad

[**intelli-monad**](https://hackage.haskell.org/package/intelli-monad) is a published Haskell library (Hackage, 0.1.3.0, April 2026) for type-safe LLM interaction via a `Prompt` state monad. It supports multiple backends (OpenAI, Anthropic, Gemini) through the `louter` routing library, has SQLite session persistence, a built-in interactive REPL, and automatic schema derivation via `GHC.Generics`.

| | **intelli-monad** | **shroom** |
|---|---|---|
| Schema derivation | `Generic` + custom `GSchema` (no TH) | `ToSchema` from `openapi3` + `Describable` |
| Sequential prompt chaining | ✓ | ✓ |
| Parallel prompts | — | ✓ `<*>` / `promptPar` via `concurrently` |
| Scoped context | — (all context globally persistent) | ✓ `WithContext` — does not leak |
| `Alternative` / fallback | — | ✓ restores original context on failure |
| Retry with LLM feedback | — (returns `Maybe`/`Either`) | ✓ auto-reprompts with error + correction |
| Session persistence | ✓ SQLite (mandatory) | — (in-memory per run) |
| Interactive REPL | ✓ with `$EDITOR` support | — |
| Streaming | ✓ | — |
| Pre/post-call hooks | ✓ | — |
| Haddock → prompt description | — (manual string literals) | ✓ TH splice, field docs auto-annotated |
| Backends | OpenAI, Anthropic, Gemini (via `louter`) | Anthropic, Ollama, FileMock |
| Built-in tools | `Bash`, `Arxiv`, `KeyValue` | `WebFetch`, `WebSearch` |
| Test coverage | Minimal | 37 unit tests + integration tests |
| Hackage | ✓ | — (not yet) |

**The Haddock advantage.** shroom's `deriveDescribable` (a TH splice) extracts the Haddock comment and all record-field docs from the type declaration at compile time, and `annotateFieldTypes` annotates each field with its JSON type from the OpenAPI schema. The result: your documentation *is* your prompt engineering — they cannot drift. intelli-monad's `HasFunctionObject` requires manually written string literals for descriptions, entirely separate from any comments.

**Schema / `FromJSON` consistency.** Both libraries require that the schema sent to the LLM and the `FromJSON` instance used to parse the response agree on the wire format — the schema instructs the LLM what to produce, and `FromJSON` deserialises what it produces. Neither library enforces this automatically. A custom `FromJSON` that expects a different encoding to what the schema describes will silently fail to parse.

### Prompt chaining best practices

shroom follows [Anthropic's prompt chaining recommendations](https://platform.claude.com/docs/en/docs/build-with-claude/prompt-engineering/chain-prompts):

- **One prompt, one type** — each `prompt @T` call targets a single type.
- **Explicit context passing** — `context` for global state, `promptWith`/`withContext` for local.
- **Validation checkpoints** — `propertyHolds` validates each response before
  the chain continues.
- **Self-correction** — on failure the model is re-prompted with the invalid response
  and a description of the violated properties.
- **Typed handoffs** — every step in the chain produces a well-typed Haskell value.

**Branching and routing** are fully supported: `prompt @SumType` returns a typed
Haskell sum type; use ordinary `case` to take different paths through the chain.

See [TODO.md](TODO.md) for planned features.

## Installation

Add to your `cabal` file:

```
build-depends: shroom
```

This package is not yet on Hackage. For now, point your `cabal.project` at the
source:

```
packages: path/to/shroom
```

## Disclaimer

No mushrooms were harmed in the making of this library. Any resemblance to
actual hallucinogens, purely coincidental. Side effects may include
well-typed outputs, reduced prompt engineering anxiety, and an inexplicable
fondness for operational monads.
