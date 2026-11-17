# shroom-baikai

The transport facts about `baikai` (constructors, `/v1` handling, `max_tokens`) are in the repo root's
`AGENTS.md`. These are what the Haddocks of `Control.Monad.Prompt.Baikai` do not say.

## `Control.Monad.Prompt.Baikai`

- **`openAICompatBackend`'s `Nothing` key is a placeholder, on purpose.** `baikai` refuses to dispatch to a host
  it has no default API-key environment variable for unless `Options.apiKey` is set, with no allowance for a
  host that checks no credential (a bare Ollama or llama.cpp server). The literal `"unused"` keeps that guard
  from firing. `anthropicCompatBackend` takes a plain `Text` key because only its caller knows what a proxy
  expects.
- **The model limits are not what bounds a reply.** `contextWindow` and `maxOutputTokens` on the `Model` are
  the published limits for `claudeBackend`'s Haiku 4.5 (200000 / 8192) and placeholders (8192 / 4096) elsewhere;
  `Options.maxTokens` (4096, always set) is the cap that matters. The Ollama measurement of why the cap must go
  out as `max_tokens` is in the root `AGENTS.md`.
- **A response is read through `responseError`, never `errorInfo`.** `responseError` is `Just` exactly when the
  stop reason is an error, also for an error stop with no classified detail; reading `errorInfo` directly would
  turn that case into a successful empty answer.
- **Tool schemas reach `baikai` unchanged.** `toBaikaiTool` does not flatten `$ref`; the schema is already flat
  when it arrives, from `schemaWithDefs` in `shroom`. Why that matters on a live Ollama is in
  `shroom/AGENTS.md`, section Schema wire facts.
- **`strict = True` on the response format** is for OpenAI, which enforces it; `schemaWithDefs` already produces
  its strict subset (`additionalProperties: false` throughout). Anthropic's structured outputs enforce the
  schema regardless.
