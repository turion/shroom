# Revision history for shroom

## Unreleased

* **Module split**: Anthropic backend moved to `Control.Monad.Prompt.Anthropic`
  (parallel to `Control.Monad.Prompt.Ollama` and `Control.Monad.Prompt.FileMock`).
* **`ContextItem`**: New `data ContextItem = SystemMessage Text | UserMessage Text | AssistantMessage Text`
  for structured conversation history. `addContextItem` / `withContextItem` for direct use;
  `context` remains the convenient shorthand.
* **`Alternative` / `MonadPlus`**: `PromptT` now implements `<|>` — try one branch,
  fall back to another on failure. Context from a failing branch does not leak.
* **`debugLog`**: `PromptConfig` now has `debugLog :: Maybe (Text -> IO ())`.
  Set to `Just putStrLn` (or `Just (step . T.unpack)` in Tasty tests) to trace
  every LLM call, context, and response.
* **`maxTokens`**: `AnthropicConfig` now has a configurable `maxTokens` field
  (default 4096). Use `mkAnthropicConfig` for a sensible default.
* **Ollama `$ref` inlining**: The Ollama backend now resolves all `$ref` pointers
  in the schema before sending (Ollama does not resolve `$ref` itself).

## 0.1.0.0 -- 2026-05-28

* First version. Released on an unsuspecting world.
