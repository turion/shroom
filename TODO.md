# shroom — planned features

The open backlog now lives in GitHub issues, <https://github.com/turion/shroom/issues>, not in
this file. What stays here are the records of decisions already taken, which are deliberately not
issues.

The direction from here, each its own arc: **C** ("shroom becomes a layer" — stop owning
transports; this is done), then **B** ([#4](https://github.com/turion/shroom/issues/4)), **A**
([#5](https://github.com/turion/shroom/issues/5)), **D**
([#6](https://github.com/turion/shroom/issues/6)), then **C3**
([#7](https://github.com/turion/shroom/issues/7)) well after. See
`~/.claude/plans/arc-shroom-layer`'s `decisions.md` for how that order was chosen.

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

# ScopedProgramT refactor — moved out

Archived as its own repository, `~/haskell/scoped-operational` (2026-09-29): a scoped,
higher-order-effect variant of `operational`'s `ProgramT`, extracted from this repo's dangling
`lyotqyrmwvst` head. It existed to give shroom's old hand-rolled program monad (see CHANGELOG for
its removal) the scoped effects that `effectful` now provides natively, so once this arc moved
shroom onto `effectful` the experiment had no consumer left. See
that repository's README for the full story; it is preserved thinking, not maintained to build.
