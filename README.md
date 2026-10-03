# shroom 🍄

> *Type-safe AI hallucinations for Haskell.*

LLMs hallucinate.
But with **shroom**, at least your hallucinations will be well-typed and guaranteed to fulfill your specs.
If the model makes something up, it'll be a properly structured something.

## What is it?

**shroom** is a Haskell library for structured LLM prompting. You write a
program against `effectful`'s `Prompt` effect using `context` and `prompt`,
pick a backend from `shroom-baikai` — the real Claude API, a model on your
own machine via Ollama, or any other OpenAI- or Anthropic-compatible host —
and get back a typed Haskell value, or a `Text` error explaining what went
wrong.

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

instance Promptable User

main :: IO ()
main = do
  backend <- claudeBackend apiKey
  result <- runPromptResultEff backend defaultPromptConfig $ do
    context "Give me a user starting with A"
    prompt @User
  print result -- Might print Right (User { userName = "Alice", userEmail = "alice@example.org" }) or something else entirely
```
The model hallucinates something. `shroom` ensures it's a type-checking `User`.

## Quickstart

`shroom` itself carries no LLM client — build a `Backend` with one of
`shroom-baikai`'s four constructors (`Control.Monad.Prompt.Baikai`) and hand
it to `runPromptResultEff`:

```haskell
-- The real Claude API:
backend <- claudeBackend apiKey

-- A model on your own machine, through Ollama's OpenAI-compatible endpoint:
backend <- localOllamaBackend "qwen3:8b"

-- Any other OpenAI-compatible host (a hosted one, or a self-hosted vLLM / llama.cpp server):
backend <- openAICompatBackend "https://my-host.example" "my-model" (Just "my-api-key")

-- Any other Anthropic-compatible host:
backend <- anthropicCompatBackend "https://my-anthropic-proxy.example" "my-model" "my-api-key"
```

`openAICompatBackend`'s key is `Maybe Text`: pass `Nothing` for a host that
checks no credential at all (a bare local Ollama or llama.cpp server).
`anthropicCompatBackend`'s key is a plain `Text` — every Anthropic-compatible
host, hosted or local, wants something in that slot.

For the local case: install and start [Ollama](https://ollama.com), then run
`ollama pull qwen3:8b` before the first call — `qwen3:8b` is small enough for
a laptop and strong enough at structured output and tool calls to be worth
recommending. Running the server declaratively on NixOS instead? This
flake's `nixosModules.ollama-shroom` pulls a model on activation and can
keep it resident between runs — but its own default is sized for a small CI
server, not a laptop, so set `services.ollama.shroom.model = "qwen3:8b";` to
match the recommendation above.

Want a different Claude model than `claudeBackend`'s default? Point
`anthropicCompatBackend` at the real API instead of a proxy —
`anthropicCompatBackend "https://api.anthropic.com" "claude-sonnet-5" apiKey`
talks to the same official endpoint with a model id of your choice.

No key, no network, at all? `Control.Monad.Prompt.FileMock`'s
`fileMockBackend` reads canned JSON responses from files on disk and prints
the exact prompt text it would have sent — the way to see what shroom
actually generates without calling anything.

Writing your own adapter instead of using `shroom-baikai` at all?
`Control.Monad.Prompt.Backend`'s module Haddock explains the single-function
`Backend` interface to satisfy — reach for it if `shroom-baikai`'s own
dependency on the `baikai` family ever becomes a problem.

## The typeclass hierarchy

shroom uses three typeclasses that form a hierarchy:

```
Describable          — describeType: one sentence (auto-derived from Haddock)
    └── Surveyable   — Property, propertyHolds, describeProperties, examples (user-filled)
            └── Promptable (+ ToSchema, FromJSON)  — prompt: the program that requests a value
```

- **`Describable`** — attach a one-sentence description to a type.
  Use `$(deriveDescribable ''MyType)` from `Control.Monad.Prompt.TH` to derive
  `describeType` automatically from your Haddock comment.

- **`Surveyable`** — the main user-filled class. Override what you need:
  - `Property` — an enumeration of properties (default: none)
  - `propertyHolds` / `describeProperties` — properties to validate after parsing
  - `examples` — sample values to include in the prompt

- **`Promptable`** — a single method, `prompt :: (Prompt :> es) => Eff es a`,
  the `effectful` program that requests a value of the type from the LLM.
  The default sends `Control.Monad.Prompt.Effect`'s `RequestPrompt`, which
  renders the prompt from the type's description, properties and examples,
  annotated with JSON field types. `instance Promptable MyType` with no body
  is the normal case; override `prompt` to add type-specific context,
  fallback logic, or any other custom program.

## Prompt chaining

`Eff` is a monad, so you can chain multiple prompts together. Each call to
`prompt` or `promptWith` is a separate LLM request, but they share the same
accumulated context:

```haskell
result <- runPromptResultEff backend defaultPromptConfig $ do
  context "The user Alice is an expert programmer:"
  user  <- prompt @User                                         -- first LLM call
  score <- promptWith @Score "Rate this user's awesomeness."   -- second LLM call, still sees "The user Alice is an expert programmer:"
  pure (user, score)
```

## Parallel prompts

Parallelism is explicit, never hidden inside an operator. Plain `<*>` runs
sequentially, the same as any other `Applicative` — reach for `promptPar` or
`promptsParallel` to actually run independent prompts concurrently, via
`effectful`'s `Concurrent` effect:

```haskell
-- Two independent prompts, run concurrently:
(user, score) <- promptPar (prompt @User) (prompt @Score)

-- A list of independent prompts, all in parallel:
talks <- promptsParallel [ promptWith @Talk ("Speaker: " <> speakerName spk) | spk <- speakers ]
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

## Falling back with `orElse`

`Eff` has no `Alternative` instance, so shroom doesn't claim one — there is no
`<|>`. Use `orElse` to try one program and fall back to another if it fails;
it restores context to how it stood before the failing attempt, so nothing
the failing branch added leaks to the fallback or to subsequent steps:

```haskell
result <- runPromptResultEff backend defaultPromptConfig $
  prompt @RichUser `orElse` fmap toRichUser (prompt @SimpleUser)
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
| Multiple LLM backends | — | ✓ (`shroom-baikai`: Claude, any OpenAI-/Anthropic-compatible host — including local Ollama) |
| Code generation | ✓ | — |
| Haddock → prompt description | — | ✓ (TH splice, field docs auto-annotated) |
| Production-ready | proof-of-concept | closer |
| Embedding | own DSL | full Haskell |
| `FromJSON` / schema must agree | n/a | ✓ (not enforced automatically — must be consistent) |

The main thing shroom does *not* have from Grace is automatic schema inference —
you write `ToSchema` and `Describable`/`Surveyable` instances yourself (or derive them). In
exchange you get property validation, automatic retry with feedback, and backends
that actually work today.

### Where shroom sits now

shroom was last touched in mid-2026, and the Haskell LLM-library field moved underneath it in the
meantime. The two projects an earlier version of this README benchmarked against —
[**intelli-monad**](https://hackage.haskell.org/package/intelli-monad) and
[**louter**](https://hackage.haskell.org/package/louter) — have both been dormant since
2026-05-05. What actually occupies this space now is **`baikai` + `shikumi`** (Nadeem Bitar, 18
Hackage packages since May 2026) and a ground-up-rewritten **`langchain-hs`** 0.0.5.0.

Racing either on their own ground is a race shroom loses: streaming, compaction, multimodal
input, routing, budget control, evaluation and MCP are already theirs, with more hands on them
than this project has. So shroom stopped trying to be a competing framework and became the layer
underneath one: `shroom` turns a documented Haskell type into what an LLM needs — prompt text, a
JSON schema, properties to validate against, and the correction message when they're violated —
and depends on no LLM client of its own; `shroom-baikai` supplies the transport, built on the same
`baikai` family shikumi itself sits on.

What shroom has that neither of them do is the Haddock-to-prompt TH splice: `deriveDescribable`
extracts a type's documentation and its record-field docs at compile time, so the documentation
*is* the prompt engineering and the two cannot drift. shikumi's `Signature` instructions and
`FieldMeta` descriptions are hand-written `Text`; so are langchain-hs's. That makes both
potential **consumers** of shroom's pure half (`Data.Shroom.*`, the part of the library that
depends on neither `effectful` nor a transport) rather than only rivals — a planned
`shroom-shikumi` package feeds shikumi's own instruction and validation machinery from the same
types shroom already describes, instead of shikumi's callers writing that prose twice.

| | shikumi | langchain-hs | shroom |
|---|---|---|---|
| Field/tool descriptions | hand-written `Text` (`FieldMeta`) | hand-written `Text` | derived from Haddock (`deriveDescribable`) |
| Providers | via the `baikai` family | Gemini, Ollama, OpenAI (no Claude) | via `shroom-baikai`: Claude, any OpenAI-/Anthropic-compatible host |
| Streaming, compaction, multimodal, routing, budgets, evaluation, MCP | ✓ (its ground) | partial | not attempted — see above |
| Property validation + retry | — | — | ✓ (`Surveyable` / `propertyHolds`, auto re-prompt) |
| A program's tool surface is in its type | — | — | ✓ (`Tool t :> es`; a program cannot call a tool it wasn't given) |

**Schema / `FromJSON` consistency.** None of the three enforce automatically that the schema sent
to the LLM and the type used to parse its response agree on the wire format — the schema instructs
the LLM what to produce, and the codec deserialises what it produces. A custom `FromJSON` that
expects a different encoding than the schema describes will silently fail to parse. (Arc B on
[TODO.md](TODO.md) is one definition yielding both, which would close this gap for shroom itself.)

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

Add both packages to your `cabal` file — `shroom` alone can describe a type and build a prompt,
but reaches no provider at all without an adapter:

```
build-depends: shroom, shroom-baikai
```

Neither is on Hackage yet (see [TODO.md](TODO.md), arc A). For now, point your `cabal.project` at
the source, which holds both as sibling directories:

```
packages:
  path/to/shroom
  path/to/shroom-baikai
```

## Disclaimer

No mushrooms were harmed in the making of this library. Any resemblance to
actual hallucinogens, purely coincidental. Side effects may include
well-typed outputs, reduced prompt engineering anxiety, and an inexplicable
fondness for operational monads.
