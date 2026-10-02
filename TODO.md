# shroom — planned features

The direction from here, each its own arc: **C** ("shroom becomes a layer" — stop owning
transports; this is what the sections below assume is already done), then **B**, **A**, **D**,
then **C3** well after. See `~/.claude/plans/arc-shroom-layer`'s `decisions.md` for how that order
was chosen.

## Arc B — one definition yielding schema and codec together

Since the LLM is the only consumer of a schema, keeping it and the `FromJSON`/`ToJSON` codec from
drifting apart matters more than which representation is chosen. Replace `openapi3`/`ToSchema`
plus a hand-written `FromJSON`/`ToJSON` pair with a single declaration that derives both
(`autodocodec`-style — `llm-simple`, dismissed as a base during arc C, takes exactly this
approach). Arc C's house rule deliberately left this alone: no todo there changed how a user
declares a schema, so this is still to do.

## Arc A — publish on Hackage

`shroom` and `shroom-baikai` are not yet released.

## Arc D — `Improve` and informal properties

* Add `informalProperties :: [Text]` (to `Surveyable`, or a sibling class) for properties that
  can't be checked algorithmically, e.g. "the name should be in English".
* Add an operation to the `Prompt` vocabulary — something like
  `improve :: (Surveyable a) => a -> Eff es (Maybe a)` — that asks the model whether a value's
  properties (informal ones included) hold, and if not, proposes a corrected value.

## Arc C3 — `shroom-shikumi`

Bridge `Data.Shroom.*` (the pure half: `Describable`/`Surveyable`, schema, prompt text) into
`shikumi`'s `Signature`/`FieldMeta` machinery as a consumer, not a base — shikumi hand-writes what
shroom derives from Haddock. Lands after B, A and D, and needs the pure/effectful module boundary
`Data.Shroom.` holds to, kept intact by all three.

## Decided against, during "shroom becomes a layer" (2026-10)

Each dropped because `shikumi` and/or `langchain-hs` already ship it, and arriving second at a
feature they already have is not a strategy:

* **Streaming** — token-by-token responses.
* **Compaction** — trimming or summarising context once it grows too large.
* **Multimodal inputs/generation** — images or audio alongside text context.
* **Routing** — a dedicated routing/orchestration layer. `prompt @SumType` plus an ordinary `case`
  already covers type-level branching, which is a smaller and different claim.
* **The Louter backend** — depending on `louter` for all transport instead of writing adapters of
  our own. Rejected: `louter` has been untouched since 2026-05-05, and `shroom-baikai` already
  reaches Claude, any OpenAI-compatible host and any Anthropic-compatible host without it.

## Still open, not yet its own arc

* **Interactive REPL** — à la `intelli-monad`: run a `Prompt` program step-by-step, inspect
  accumulated context, and edit the current prompt in `$EDITOR` before sending.
* **Hook system** — pre/post-call hooks: register side-effectful callbacks that fire before and
  after each LLM call, for logging, metrics, rate-limiting or custom retry logic external to the
  core framework.
* **What features does Anthropic and/or Ollama have that we're currently not using?**
* **Planning/thinking mode.**
* **Backend-specific features** — a typeclass constraint restricting which backends a given
  `Promptable` instance may run against, for a capability not every provider has.
* **Tool use follow-up:**
  * **OCR tool** — separate cabal package `shroom-ocr` with bindings to the best OCR library
    available; the model may call OCR on an image.
  * **Key-value storage tool** — the user may store e.g. an image, a longer text or a file in a
    key-value store living on the user's side; the model may retrieve, write, rename or delete
    entries in it.
  * **Local file manipulation tool.**

# ScopedProgramT refactor — moved out

Archived as its own repository, `~/haskell/scoped-operational` (2026-09-29): a scoped,
higher-order-effect variant of `operational`'s `ProgramT`, extracted from this repo's dangling
`lyotqyrmwvst` head. It existed to give shroom's old hand-rolled program monad (see CHANGELOG for
its removal) the scoped effects that `effectful` now provides natively, so once this arc moved
shroom onto `effectful` the experiment had no consumer left. See
that repository's README for the full story; it is preserved thinking, not maintained to build.
