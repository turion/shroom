# shroom 🍄

> *Type-safe AI hallucinations for Haskell.*

LLMs hallucinate. That's basically their whole thing. But with **shroom**, at
least your hallucinations will be well-typed, `FromJSON`-parsed, and validated
against your invariants. If the model makes something up, it'll be a *properly
structured* something.

## What is it?

**shroom** is a small Haskell library for structured LLM prompting. You write a
monadic program using `context` and `prompt`, pick a backend
(Anthropic Claude or a local Ollama model), and get back a typed Haskell value
— or a `Text` error explaining what went wrong.

```haskell
data User = User { userName :: Text, userEmail :: Text }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

instance Describe User where
  describeType = $(deriveDescribeType ''User)
```

```haskell
result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig $ do
  context "The user's name is Alice and her email is alice@example.com."
  prompt @User

-- result :: Either Text User
```

The model hallucinates a `User`. The library parses it. You get a `Right User`.
Everyone wins.

## Backends

| Backend | Config type | Module | Notes |
|---|---|---|---|
| Anthropic Claude | `AnthropicConfig` | `Control.Monad.Prompt.Anthropic` | Requires `ANTHROPIC_API_KEY`; uses structured JSON output |
| Ollama (local) | `OllamaBackendConfig` | `Control.Monad.Prompt.Ollama` | Free, offline, slower; defaults to `llama3.2:3b` |
| File mock | `FileMockConfig` | `Control.Monad.Prompt.FileMock` | Reads canned JSON responses from files; useful for dev/testing |

## The `Describe` typeclass

`Describe` lets you attach three things to a type:

- **`describeType`** — a one-sentence description sent to the model
- **`propertyHolds`** / **`describeProperties`** — invariants to validate after parsing
- **`examples`** — sample values to include in the prompt

Use `deriveDescribeType` from `Control.Monad.Prompt.TH` to derive
`describeType` automatically from your Haddock comment.

## Prompt chaining

`PromptT` is a monad, so you can chain multiple prompts together. Each call to
`prompt` or `promptWith` is a separate LLM request, but they share the same
accumulated context:

```haskell
result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig $ do
  context "Alice is 30 years old."
  user  <- prompt @User                                         -- first LLM call
  score <- promptWith @Score "Rate this user's awesomeness."   -- second LLM call, still sees "Alice is 30..."
  pure (user, score)
```

## Parallel prompts

Independent prompts can run concurrently. The `Applicative` instance uses
`unliftio`'s `concurrently` under the hood, so both LLM calls are issued at
the same time:

```haskell
-- Two independent prompts run concurrently:
(user, score) <- promptPar (prompt @User) (prompt @Score)

-- A list of independent prompts, all in parallel:
talks <- promptsParallel
  [ promptWith @Talk ("Speaker: " <> speakerName spk) | spk <- speakers ]
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

Grace is a proof-of-concept research prototype (not production-ready). It lives on
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
| Property validation + retry | — | ✓ (`Describe` / `propertyHolds`) |
| Parallel prompts | — | ✓ (`promptPar`, `promptsParallel`) |
| Multiple LLM backends | — | ✓ (Claude, Ollama, FileMock) |
| Code generation | ✓ | — |
| Production-ready | proof-of-concept | closer |
| Embedding | own DSL | full Haskell |

The main thing shroom does *not* have from Grace is automatic schema inference —
you write `ToSchema` and `Describe` instances yourself (or derive them). In
exchange you get property validation, automatic retry with feedback, and backends
that actually work today.

### Prompt chaining best practices

shroom follows [Anthropic's prompt chaining recommendations](https://platform.claude.com/docs/en/docs/build-with-claude/prompt-engineering/chain-prompts):

- **One prompt, one type** — each `prompt @T` call targets a single type.
- **Explicit context passing** — `context` for global state, `promptWith` for local.
- **Validation checkpoints** — `propertyHolds` validates each response before
  the chain continues.
- **Self-correction** — on failure the model is re-prompted with the invalid response
  and a description of the violated invariants.
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
