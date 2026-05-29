# shroom — planned features

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
 
## Streaming

## What features does anthropic and/or ollama have that we're currently not using?

## Compaction
