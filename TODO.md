# shroom — planned features

## Simplification: Completely generic schema

Since the LLM is the only one ever seeing the data in a schema,
the particular chosen schema doesn't matter.
We could just require Generic and derive a generic schema and ToJSON/FromJSON instances
that are always compatible.

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

Support streaming LLM responses token-by-token. Would require extending `LLMBackend`
with a streaming method variant and threading a callback or conduit through `runPromptT`.
Both Anthropic and Ollama support streaming. Mainly useful for long-form outputs and
interactive UX.

## Interactive REPL

An interactive REPL (à la intelli-monad) allowing the user to run `PromptT` programs
step-by-step, inspect accumulated context, and edit the current prompt in `$EDITOR`
before sending. Useful for iterative prompt development and debugging without writing
test files.

## Hook system

Pre/post-call hooks: a `Hook` mechanism allowing users to register side-effectful
callbacks that fire before and after each LLM call. Useful for logging, metrics,
rate-limiting, and custom retry logic external to the core framework.

## What features does anthropic and/or ollama have that we're currently not using?

## Compaction

## Backend specific features

Typeclass constraint on the backend possible in the prompt definition.
E.g. URL images in anthropic


## Image/multimodal generation

## Tool use follow up

### OCR tool

Separate cabal package shroom-ocr with bindings to the best ocr library available.
LLM may call OCR on an image.

### Key value storage tool

* User may store e.g. an image or a longer text or a file in a key value store living on the user side
* LLM may retrieve, write, rename, delete etc. the key value store

### Local file manipulation tool


### Tool example

Implement a "Facts about a celebrity" tool use example.
The LLM is first instructed to offer a list of 3 celebrities.
User chooses one of them randomly.
Next step: LLM should read their wikipedia article and summarize one trivia fact.


# Adding structured data to the prompt

# ScopedProgramT refactor


-- The only difference: spm parameter here
data Instr spm a where ...

data ScopedProgramT instr m a where
  Lift, Bind -- as usual
  
  -- Shallow effect handler
  Instr :: instr (ScopedProgramT m) a -> ProgramT instr m a


# Louter backend

Might even explore whether we want to completely depend on louter and always use it.
Simplifies architecture. Does it support ollama?
