{-# LANGUAGE OverloadedRecordDot #-}

{- | The @baikai@ adapter: implements
"Control.Monad.Prompt.Backend"\'s 'Backend' interface by interpreting
@baikai-effectful@\'s @Baikai@ effect, one 'complete' call per
'runBackendChat'. Dispatching a tool call and feeding its result back in —
the actual loop — stays the caller's job
("Control.Monad.Prompt.Effect"\'s @toolLoop@), same as every other
'Backend'; this module only ever makes one call.

Four front-door constructors cover what "Control.Monad.Prompt.Effect" needs
to run against: 'claudeBackend' for the real Claude API and
'anthropicCompatBackend' for any other Anthropic-compatible host (both via
@baikai-claude@), 'localOllamaBackend' for a local Ollama server, and
'openAICompatBackend' for any other OpenAI-compatible host (both of the
latter two via @baikai-openai@). Each calls the matching provider's
@register@ once, fills in the 'Model' limits @baikai@\'s own 'mkModel'
otherwise leaves at zero, and makes its own call on whether the target
needs a real API key.
-}
module Control.Monad.Prompt.Baikai (
  -- * Front door
  claudeBackend,
  anthropicCompatBackend,
  localOllamaBackend,
  openAICompatBackend,

  -- * The adapter itself
  baikaiBackend,
  contextItemsToBaikai,
  normaliseOllamaHost,
) where

-- base
import Control.Exception (SomeAsyncException, SomeException, fromException, tryJust)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (toList)
import System.Environment (lookupEnv)

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

-- effectful
import Effectful (runEff)

-- baikai
import Baikai.Api (Api (AnthropicMessages, OpenAIChatCompletions))
import Baikai.Auth (ApiKeySource (ApiKeyLiteral))
import Baikai.Content (AssistantContent (AssistantToolCall))
import Baikai.Content qualified as BContent
import Baikai.Context (Context (messages, systemPrompt, tools), emptyContext)
import Baikai.Error (ErrorCategory (ContentFiltered))
import Baikai.Error qualified as BE
import Baikai.Message (AssistantPayload (..), Message, assistant, toolResultErrorText, toolResultMessage, toolResultText, user)
import Baikai.Message qualified as BMessage
import Baikai.Model (Model (..), mkModel)
import Baikai.Options (Options (..), emptyOptions)
import Baikai.Response (Response (..), flattenAssistantText, responseError)
import Baikai.ResponseFormat (JsonSchemaFormat (strict), ResponseFormat (JsonSchema), jsonSchemaFormat)
import Baikai.StopReason (StopReason (ToolUse))
import Baikai.Tool (Tool, mkTool)
import Baikai.Usage (zeroUsage)

-- baikai-effectful
import Baikai.Effectful (complete, runBaikai)

-- baikai-claude
import Baikai.Provider.Claude.Api qualified as ClaudeProvider

-- baikai-openai
import Baikai.Provider.OpenAI.Api qualified as OpenAIProvider

-- shroom
import Control.Monad.Prompt.Backend (
  Backend (..),
  BackendError (..),
  BackendReply (..),
  ContextItem (..),
  ToolCall (..),
  ToolDef (..),
  ToolResult (..),
 )

-- * Converting shroom's context to baikai's

{- | Convert shroom's 'ContextItem' history plus the type-description turn to
a baikai 'Context'. 'ContextItem'\'s five constructors map onto baikai's
own message shapes one for one: 'SystemMessage' items collect into
@systemPrompt@ (there is only one slot, so several are joined with
newlines, the same joining the deleted @Control.Monad.Prompt.Anthropic@
module's old @contextItemsToAnthropic@ used before this adapter replaced it);
'ToolCallMessage' becomes one 'AssistantMessage' carrying an
'AssistantToolCall' block per call, with 'zeroUsage' and 'ToolUse' as its
@stopReason@ since neither is known once a call has round-tripped through
'ContextItem'; 'ToolResultMessage' becomes one @ToolResultMessage@ per
result. The @tools@ field is left empty here — it is the same for every
turn, so 'baikaiBackend' sets it once rather than threading it through this
conversion.
-}
contextItemsToBaikai :: [ContextItem] -> Text -> Context
contextItemsToBaikai items typeDesc =
  emptyContext
    { systemPrompt = case [t | SystemMessage t <- items] of
        [] -> Nothing
        ts -> Just (T.intercalate "\n" ts)
    , messages =
        V.fromList $
          concatMap toBaikaiMessages [item | item <- items, not (isSystemItem item)]
            <> [user typeDesc]
    }
  where
    isSystemItem (SystemMessage _) = True
    isSystemItem _ = False

toBaikaiMessages :: ContextItem -> [Message]
toBaikaiMessages (SystemMessage _) = []
toBaikaiMessages (UserMessage t) = [user t]
toBaikaiMessages (AssistantMessage t) = [assistant t]
toBaikaiMessages (ToolCallMessage calls) =
  [ BMessage.AssistantMessage
      AssistantPayload
        { content = V.fromList (AssistantToolCall . toBaikaiToolCall <$> calls)
        , usage = zeroUsage
        , stopReason = ToolUse
        , errorMessage = Nothing
        , timestamp = Nothing
        }
  ]
toBaikaiMessages (ToolResultMessage results) = fmap toBaikaiToolResultMessage results

toBaikaiToolCall :: ToolCall -> BContent.ToolCall
toBaikaiToolCall tc =
  BContent.ToolCall
    { BContent.id_ = tc.toolCallId
    , BContent.name = tc.toolCallName
    , BContent.arguments = tc.toolCallArguments
    }

toBaikaiToolResultMessage :: ToolResult -> Message
toBaikaiToolResultMessage tr =
  toolResultMessage tr.toolResultId tr.toolResultName (toBaikaiToolResult tr.toolResultOutcome)
  where
    toBaikaiToolResult :: Either Text Text -> BMessage.ToolResult
    toBaikaiToolResult (Left err) = toolResultErrorText err
    toBaikaiToolResult (Right txt) = toolResultText txt

{- | Convert a 'ToolDef' to a baikai 'Tool'. The schema is passed straight
through as the opaque 'Data.Aeson.Value' 'Tool'\'s own @parameters@ field
is typed as — unlike @ollama-haskell@\'s typed 'FunctionParameters', baikai
has no schema representation of its own to lose @$ref@\/@$defs@ to on this
side. That is not the same claim as "a @$ref@ would survive the wire if
one reached here": a live Ollama (@0.30.6@) resolves only one @$ref@ hop
and silently stops enforcing the schema on a second, which any nested
tool argument would produce. This function never sees that case only
because every schema reaching a 'ToolDef' is already flattened upstream,
by 'Control.Monad.Prompt.Schema.schemaWithDefs' — see that function's
Haddock for the wire evidence. 'toBaikaiTool' itself does nothing to
guarantee a flat schema; it just happens to always be handed one today.
-}
toBaikaiTool :: ToolDef -> Tool
toBaikaiTool td = mkTool td.toolDefName td.toolDefDescription td.toolDefSchema

-- * Reading baikai's response back

{- | Read a 'Response' back into a 'BackendReply' \/ 'BackendError'.

Goes through baikai's own 'responseError' rather than pattern-matching
'errorInfo' directly — it is, per its own Haddock, "the single question
callers ask about failure": 'Just' exactly when @stopReason = ErrorReason@,
be that the provider's classified 'errorInfo' or, should a provider ever
report an error stop with no classified detail, a synthesized @OtherError@
built from the response's own error text. Reading 'errorInfo' directly
would treat that synthesized case as no error at all — an @ErrorReason@
response silently read as a successful empty answer.

A 'ContentFiltered' category is what baikai gives "content the provider
refused or filtered" — a refusal — so it becomes 'BackendRefusal',
carrying whatever 'refusalCategory' baikai populated (only ever non-'Nothing'
on Anthropic's streaming path, per 'BackendError'\'s own Haddock — this
adapter uses baikai's non-streaming 'complete', so it is 'Nothing' in
practice, but the field is threaded through regardless). Any other
category (auth, rate limit, decode failure, ...) is exactly what
a retry is for, so it becomes 'BackendTransportError'. With no error, any
'AssistantToolCall' blocks in the reply mean 'BackendToolCalls'; otherwise
it is a final 'BackendAnswer', read via 'flattenAssistantText'.
-}
interpretResponse :: Response -> Either BackendError BackendReply
interpretResponse resp = case responseError resp of
  Just err
    | BE.category err == ContentFiltered ->
        Left (BackendRefusal {refusalCategory = BE.refusalCategory err, refusalMessage = BE.message err})
    | otherwise -> Left (BackendTransportError (BE.message err))
  Nothing ->
    let calls = [tc | AssistantToolCall tc <- toList resp.message.content]
     in if null calls
          then Right (BackendAnswer (flattenAssistantText resp.message.content))
          else Right (BackendToolCalls (fmap fromBaikaiToolCall calls))

fromBaikaiToolCall :: BContent.ToolCall -> ToolCall
fromBaikaiToolCall bc =
  ToolCall
    { toolCallId = BContent.id_ bc
    , toolCallName = BContent.name bc
    , toolCallArguments = BContent.arguments bc
    }

-- * The adapter

{- | Build a 'Backend' that makes one @baikai-effectful@ 'complete' call per
'runBackendChat', against the given 'Model' and base 'Options'. The three
front-door constructors below are built on this; reach for it directly only
if none of them fit (a fourth provider, or non-default 'Options' this
module does not expose a knob for).

@responseFormat@ is set on every call, tools or no — matching the deleted
@Control.Monad.Prompt.Anthropic@ module's old behaviour of always requesting
structured output and letting the provider ignore it when a call stops for
a tool call instead. @strict = True@: OpenAI honours it and it costs
nothing extra since 'Control.Monad.Prompt.Schema.schemaWithDefs' already
produces schema OpenAI's strict subset accepts (@additionalProperties:
false@ throughout); Anthropic's structured outputs are always
schema-enforcing and ignore the flag either way.

Per 'Backend'\'s own contract, no /synchronous/ 'SomeException' a 'complete'
call can throw escapes this function — it comes back as
'BackendTransportError' instead. /Asynchronous/ exceptions
('SomeAsyncException': @System.Timeout.timeout@\'s @Timeout@, @ThreadKilled@,
@UserInterrupt@, an @async@ cancellation) are the caller's control flow, not
a transport failure, so they propagate: a @timeout@ around a hung call
returns 'Nothing' instead of the prompt loop retrying it as an ordinary
backend error.
-}
baikaiBackend :: (MonadIO m) => Model -> Options -> Backend m
baikaiBackend model baseOpts =
  Backend $ \ctx typeDesc schema toolDefs -> liftIO $ do
    let bCtx = (contextItemsToBaikai ctx typeDesc) {tools = V.fromList (fmap toBaikaiTool toolDefs)}
        opts =
          baseOpts
            { responseFormat = Just (JsonSchema ((jsonSchemaFormat "response" schema) {strict = True}))
            }
    result <- tryJust synchronousOnly (runEff (runBaikai (complete model bCtx opts)))
    pure $ case result of
      Left ex -> Left (BackendTransportError (T.pack (show ex)))
      Right resp -> interpretResponse resp
  where
    -- 'Nothing' lets an asynchronous exception keep propagating; 'Just'
    -- catches the synchronous ones.
    synchronousOnly :: SomeException -> Maybe SomeException
    synchronousOnly ex = case fromException ex of
      Just (_ :: SomeAsyncException) -> Nothing
      Nothing -> Just ex

-- * Front door

{- | The Anthropic wiring shared by 'claudeBackend' and
'anthropicCompatBackend': registers @baikai-claude@\'s provider once and
builds the bare 'AnthropicMessages' 'Model' for the given base URL and model
id, @contextWindow@ \/ @maxOutputTokens@ left at @baikai@\'s own zeroed
defaults. Each public constructor record-updates the result with its own
limits — this helper never picks any itself, so the two call sites cannot
drift apart by one of them forgetting to override a field the other set.
-}
anthropicModel :: Text -> Text -> IO Model
anthropicModel baseUrl modelId = do
  ClaudeProvider.register
  pure (mkModel AnthropicMessages modelId baseUrl)

{- | The real Claude API, via @baikai-claude@. Takes your Anthropic API key;
defaults to @claude-haiku-4-5-20251001@ and 4096 max output tokens, matching
the deleted @Control.Monad.Prompt.Anthropic@ module's old @mkAnthropicConfig@
defaults. The 'Model'\'s @contextWindow@ \/ @maxOutputTokens@ (200000 \/
8192) are Claude Haiku 4.5's published limits, not @baikai@\'s own zeroed
defaults. This constructor does not hand back the 'Model' to override them
for a different Claude model — reach for 'anthropicCompatBackend' instead,
pointed at the same host: @anthropicCompatBackend \"https:\/\/api.anthropic.com\"
\"claude-sonnet-5\" key@. Its placeholder @contextWindow@ \/
@maxOutputTokens@ (8192 \/ 4096, see that function's Haddock) don't truncate
anything either — @maxTokens@ in 'Options' is what actually bounds a
response, and it is always set explicitly.
-}
claudeBackend :: (MonadIO m) => Text -> IO (Backend m)
claudeBackend apiKey = do
  model <- anthropicModel "https://api.anthropic.com" "claude-haiku-4-5-20251001"
  let model' = model {contextWindow = 200000, maxOutputTokens = 8192}
      opts = emptyOptions {apiKey = Just (ApiKeyLiteral apiKey), maxTokens = Just 4096}
  pure (baikaiBackend model' opts)

{- | Any Anthropic-compatible host, via @baikai-claude@ — the Anthropic mirror
of 'openAICompatBackend'. Takes the base URL (no trailing @\/v1@:
'Baikai.Http.canonicalBaseUrl' strips one, and the transport appends its
own, so giving one here would compose to @\/v1\/v1\/...@), the upstream
model id, and an API key.

Unlike 'openAICompatBackend'\'s key, this one is a plain 'Text', not a
'Maybe'. @baikai@\'s keyless-host guard — refusing to dispatch to a host it
has no default credential for unless an 'Baikai.Options.apiKey' is set —
is 'openAICompatBackend'\'s problem to solve because a bare local Ollama or
llama.cpp server genuinely checks no credential at all. An
Anthropic-compatible host is a different proposition: a hosted one checks a
real key, and a local proxy that doesn't still wants a placeholder — but one
the caller supplies, since only the caller knows what the target expects,
if anything. So this constructor takes whatever 'Text' you hand it and
passes it straight through, unconditionally.

'contextWindow' \/ 'maxOutputTokens' get the same conservative,
clearly-a-placeholder defaults 'openAICompatBackend' uses (8192 \/ 4096)
since neither is knowable without knowing which model is actually served;
override with a record update once you do, via 'baikaiBackend' directly
with your own 'Model'.
-}
anthropicCompatBackend :: (MonadIO m) => Text -> Text -> Text -> IO (Backend m)
anthropicCompatBackend baseUrl modelId apiKey = do
  model <- anthropicModel baseUrl modelId
  let model' = model {contextWindow = 8192, maxOutputTokens = 4096}
      opts = emptyOptions {apiKey = Just (ApiKeyLiteral apiKey), maxTokens = Just 4096}
  pure (baikaiBackend model' opts)

{- | A local Ollama server, reached through @baikai-openai@'s OpenAI-compatible
Chat Completions client (baikai has no dedicated Ollama provider; Ollama's
own server speaks the OpenAI Chat Completions wire format well enough that
none is needed).

Honours @OLLAMA_HOST@ the same way Ollama's own @envconfig.Host()@ does,
via 'normaliseOllamaHost': unset defaults to @http:\/\/127.0.0.1:11434@; a
bare @host@ or @host:port@ (Ollama's own convention, no scheme) gets
@http:\/\/@ prepended, with port @11434@ supplied only when the value did
not already name one — 'Baikai.Http.canonicalBaseUrl' would otherwise
default a schemeless, portless host to port 80. A value that already names
a scheme (@https:\/\/tunnel-host@, say, for a TLS-terminating proxy in
front of a remote Ollama) passes through untouched, port and all.

Ollama itself checks no credential at all, so this passes 'openAICompatBackend'
'Nothing' for the API key — see that function's Haddock for what that does.
-}
localOllamaBackend :: (MonadIO m) => Text -> IO (Backend m)
localOllamaBackend modelId = do
  mHost <- lookupEnv "OLLAMA_HOST"
  let baseUrl = maybe "http://127.0.0.1:11434" (normaliseOllamaHost . T.pack) mHost
  openAICompatBackend baseUrl modelId Nothing

{- | Give an @OLLAMA_HOST@-style value a scheme, and a default port, if it
does not already have them.

Ollama's own convention — and what @OLLAMA_HOST@ is documented to hold — is
a bare @host@ or @host:port@, so a value naming no @\"scheme:\/\/\"@ gets
@http:\/\/@ prepended, with @:11434@ appended too when it named no port of
its own (mirroring Ollama's own @envconfig.Host()@, since
'Baikai.Http.canonicalBaseUrl' would otherwise default the missing port to
80, not 11434). A value that already names a scheme is left alone
entirely, port included: both Ollama's own parser and @baikai@ already
default to the scheme's standard port, and forcing 11434 unconditionally
would break a real configuration such as @OLLAMA_HOST=https:\/\/tunnel-host@,
a TLS-terminating proxy in front of a remote Ollama listening on 443.
-}
normaliseOllamaHost :: Text -> Text
normaliseOllamaHost h
  | "://" `T.isInfixOf` h = h
  | ":" `T.isInfixOf` h = "http://" <> h
  | otherwise = "http://" <> h <> ":11434"

{- | Any OpenAI-compatible host — a real hosted OpenAI-compatible API, or a
self-hosted one such as vLLM or llama.cpp's server — via @baikai-openai@.
Takes the base URL (no trailing @\/v1@: 'Baikai.Http.canonicalBaseUrl'
strips one, and the transport appends its own, so giving one here would
compose to @\/v1\/v1\/...@) and the upstream model id.

The 'Maybe' 'Text' is the keyless-host decision @baikai@ forces on every
caller: it refuses to dispatch to a host it has no default API key
environment variable for unless 'Baikai.Options.apiKey' is set, with no
allowance for a host that simply has no credential to check (a bare local
Ollama or llama.cpp server, say). @Nothing@ supplies a literal placeholder
so that guard never fires for such a host — the target never inspects it —
while @Just apiKey@ passes a real key through unchanged for a host that
does check one. 'contextWindow' \/ 'maxOutputTokens' get conservative,
clearly-a-placeholder defaults (8192 \/ 4096) since neither is knowable
without knowing which model is actually pulled; override with a record
update once you do (again, via 'baikaiBackend' directly with your own
'Model').
-}
openAICompatBackend :: (MonadIO m) => Text -> Text -> Maybe Text -> IO (Backend m)
openAICompatBackend baseUrl modelId mApiKey = do
  OpenAIProvider.register
  let model =
        (mkModel OpenAIChatCompletions modelId baseUrl)
          { contextWindow = 8192
          , maxOutputTokens = 4096
          }
      opts =
        emptyOptions
          { apiKey = Just (maybe (ApiKeyLiteral "unused") ApiKeyLiteral mApiKey)
          , maxTokens = Just 4096
          }
  pure (baikaiBackend model opts)
