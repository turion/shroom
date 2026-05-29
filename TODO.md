# shroom — planned features

## Configurable `max_tokens` in `AnthropicConfig`

`max_tokens` is currently hardcoded to `1024` in
`src/Control/Monad/Prompt.hs` (the `AnthropicConfig` backend).
Add a field:

```haskell
data AnthropicConfig = AnthropicConfig
  { apiKey    :: Text
  , model     :: Text
  , maxTokens :: Int   -- default 1024
  }
```

## Multimodal inputs

Support passing images (and potentially audio) alongside text context.
Requires extending the `context` / `promptWith` API to accept typed media
values, and updating backends that support vision (Claude, newer Ollama models).

## Tool use support

Declare callable tools and let the model choose which to invoke.
Grace handles this elegantly via sum types + automatic execution; shroom could
expose a declarative `Tool` abstraction so that tool definitions, schemas, and
result injection are handled by the library rather than the caller.

## Improve Ollama handling

Maybe it's something about the JSON schema that can be improved

## Nested schemas

## Anthropic API schema

Why do we need to modify so much

## Planning/thinking mode

## Informal properties

* To the Describe class, add `informalProperties :: [Text]` that are properties that we can't easily verify algorithmically, like "The name should be in english".
* To PromptT, add a constructor `Improve :: Describe a => a -> PromptT m (Maybe a)` that asks the AI whether the properties are fulfilled, and if not suggest an improved version that does fulfill them.
 
## Debug mode

Add a debug mode config variable to PromptConfig.
Run a prompt in debug mode where we can see the prompt and the schemata. In particular use this in all the tests so you'll immediately see why things are failing and what prompts/schemata were used.

## Streaming

## What features does anthropic and/or ollama have that we're currently not using?

## Compaction
