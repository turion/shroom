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
- **String `format` is sent as it is.** Anthropic's documentation lists `format` among the schema features it
  enforces (`date-time`, `date`, `time`, `duration`, `email`, `hostname`, `uri`, `ipv4`, `ipv6`, `uuid`). A
  local model behind llama.cpp is uneven: its grammar converter compiles `uuid`, `date`, `time` and
  `date-time` into grammar rules and leaves `email` and `uri` unconstrained. One live test with `format:
  email` got back `"thompsons sophia@outlook.com "`, with a stray space inside. Which host it ran against
  was not recorded; the failure's shape matches llama.cpp's converter, and a direct call to
  `api.anthropic.com` had returned a `401`, so no successful Anthropic call backs this up.
- **Open question:** whether a llama.cpp-backed grammar enforces the integer bounds numerically; they are
  folded into the field's `description` and removed from the wire. The `Surveyable` properties exist to
  check in code what a host does not enforce.

## `Control.Monad.Prompt.Generic`

- **Both instances for `Generically` are orphans, on purpose** (`{-# OPTIONS_GHC -Wno-orphans #-}`).
  `ToSchema (Generically a)` is missing from `openapi3`, which is where it belongs; it forwards to `a`'s schema
  and exists to satisfy `Promptable`'s superclass. `Promptable (Generically a)` cannot sit beside the class in
  `Control.Monad.Prompt.Promptable`, because the strategy calls that class's own `prompt` on every field and
  `improve` from `Control.Monad.Prompt.Effect`; a module of its own is worth the one orphan. `Describable` and
  `Surveyable` for `Generically` are not orphans: they are in `Data.Shroom.Class`. The context of the
  `Promptable` instance is mostly about `a`; `ToJSON (Generically a)` and `FromJSON (Generically a)` are
  `aeson`'s generic ones, needed by the superclasses.
- **`promptSingle` and `improve` are flat on purpose,** so the strategy may call `improve` without recursing
  into itself; only the class method `prompt` runs a `deriving via Generically` strategy.
