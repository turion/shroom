{-# LANGUAGE OverloadedRecordDot #-}

{- |
Module      : Control.Monad.Prompt.Baikai
Description : Backends for Claude, a local Ollama and other hosted models.

Builds the t'Control.Monad.Prompt.Backend.Backend' that sends a shroom program's
requests to a real model, through @baikai@. Pick the constructor for the host and
pass the result to 'Control.Monad.Prompt.Effect.runPromptResultEff':

@
import Control.Monad.Prompt (prompt)
import Control.Monad.Prompt.Baikai (claudeBackend)
import Control.Monad.Prompt.Effect (context, defaultPromptConfig, runPromptResultEff)

main :: IO ()
main = do
  backend <- claudeBackend apiKey
  result <- runPromptResultEff backend defaultPromptConfig $ do
    context \"A spider has eight legs.\"
    prompt \@Int
  print result
@

The example needs @OverloadedStrings@ and @TypeApplications@, and @apiKey@ is an
Anthropic API key as @Text@. For another host, replace @claudeBackend@:

* 'claudeBackend' is the Claude API.
* 'anthropicCompatBackend' is any other host that speaks the Anthropic Messages API.
* 'localOllamaBackend' is a local Ollama server.
* 'openAICompatBackend' is any other host that speaks the OpenAI Chat Completions API.

All four are built on 'baikaiBackend', which makes one request per
'Control.Monad.Prompt.Backend.runBackendChat'. Calling a tool the model asks for, and
sending the result back, is left to the caller as for every backend (see
'Control.Monad.Prompt.Effect.promptTools').

The four constructors have these in common:

* Each registers its @baikai@ provider when it runs, so each returns in 'IO'.
* The base URL of 'anthropicCompatBackend' and 'openAICompatBackend' takes no
  trailing @\/v1@. The transport appends its own, so one given
  here would produce @\/v1\/v1\/...@.
* A reply is limited to 4096 tokens. The context window and output limit set on the
  'Model' are the published ones for the model of 'claudeBackend', and the placeholders
  8192 and 4096 for the other constructors, whose model is not known; they do not
  limit a reply.

Note that other limits need 'baikaiBackend' with a 'Model' and 'Options' of your own.
-}
module Control.Monad.Prompt.Baikai (
  -- * Backends for common hosts
  claudeBackend,
  anthropicCompatBackend,
  localOllamaBackend,
  openAICompatBackend,

  -- * Custom backends
  baikaiBackend,
  contextItemsToBaikai,

  -- * Auxiliary definitions and utilities
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
import Baikai.Compat (MaxTokensField (MaxTokensField), OpenAICompletionsCompat (maxTokensField), autoDetectOpenAICompletions, hostMatchesSuffix, urlHost)
import Baikai.Content (AssistantContent (AssistantToolCall))
import Baikai.Content qualified as BContent
import Baikai.Context (Context (messages, systemPrompt, tools), emptyContext)
import Baikai.Error (ErrorCategory (ContentFiltered))
import Baikai.Error qualified as BE
import Baikai.Message (AssistantPayload (..), Message, assistant, toolResultErrorText, toolResultMessage, toolResultText, user)
import Baikai.Message qualified as BMessage
import Baikai.Model (Compat (CompatNone, CompatOpenAICompletions), Model (..), mkModel)
import Baikai.Options (Options (..), emptyOptions)
import Baikai.Response (Response (..), flattenAssistantText, responseError)
import Baikai.ResponseFormat (JsonSchemaFormat (strict), ResponseFormat (JsonSchema), jsonSchemaFormat)
import Baikai.StopReason (StopReason (Length, ToolUse))
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

{- | Turn shroom's context items plus the type-description turn into the request 'Context' of baikai, for a hand-written t'Backend' built on baikai.

Its 'SystemMessage' items are joined with newlines into the one
@systemPrompt@. 'ToolCallMessage' becomes one assistant message with a tool-call
block per call, and 'ToolResultMessage' one tool-result message per result. The
@tools@ field stays empty, for 'baikaiBackend' to set.
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

{- | Convert a t'ToolDef' to a baikai 'Tool'. The schema is passed on unchanged as the
opaque 'Data.Aeson.Value' that the @parameters@ field of 'Tool' holds. It must
already be flat, without @$ref@; every schema a t'ToolDef' carries is.
-}
toBaikaiTool :: ToolDef -> Tool
toBaikaiTool td = mkTool td.toolDefName td.toolDefDescription td.toolDefSchema

-- * Reading baikai's response back

{- | Read a t'Response' as a 'BackendReply' or a 'BackendError'.

Failure is decided by baikai's 'responseError', not by 'errorInfo': it is 'Just'
exactly when the stop reason is an error, including an error stop with no classified
detail, which 'errorInfo' alone would read as a successful empty answer.

A 'ContentFiltered' error is a refusal and becomes 'BackendRefusal'. Its
'refusalCategory' is populated only on Anthropic's streaming path, so with the
non-streaming 'complete' it is 'Nothing'. Any other error is a transport failure
for the retry to handle: 'BackendTransportError'. A 'Length' stop is a reply cut off
by the token or context limit and becomes 'BackendTruncated'. Otherwise tool-call
blocks in the reply make 'BackendToolCalls', and none make a 'BackendAnswer'.
-}
interpretResponse :: Response -> Either BackendError BackendReply
interpretResponse resp = case responseError resp of
  Just err
    | BE.category err == ContentFiltered ->
        Left (BackendRefusal {refusalCategory = BE.refusalCategory err, refusalMessage = BE.message err})
    | otherwise -> Left (BackendTransportError (BE.message err))
  Nothing
    -- Cut off by the token or context limit: whatever arrived, text or
    -- tool-call arguments, is half a document.
    | resp.message.stopReason == Length ->
        Left (BackendTruncated (flattenAssistantText resp.message.content))
    | otherwise ->
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

{- | Build a t'Backend' that makes one @baikai-effectful@ 'complete' call per
'runBackendChat', against the given 'Model' and base 'Options'. The four constructors
of this module are built on it; use it directly for another provider, or for 'Options'
they do not set.

The schema of the requested type goes out as the structured-output format on every
call, with or without tools, and with @strict = True@: OpenAI enforces it, and the
schema shroom produces already fits OpenAI's strict subset. Anthropic's structured
outputs enforce the schema either way.

No /synchronous/ exception a 'complete' call throws escapes this function; it comes
back as 'BackendTransportError'. /Asynchronous/ exceptions
('SomeAsyncException': a @System.Timeout.timeout@, @ThreadKilled@, an @async@
cancellation) propagate, so that a @timeout@ around a hung call returns 'Nothing'
instead of being retried as a backend error.
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

-- * Backends for common hosts

{- | The 'Model' for the Anthropic constructors, with @baikai@'s own zero limits for
the caller to fill in. Registers the @baikai-claude@ provider.
-}
anthropicModel :: Text -> Text -> IO Model
anthropicModel baseUrl modelId = do
  ClaudeProvider.register
  pure (mkModel AnthropicMessages modelId baseUrl)

{- | The Claude API at @api.anthropic.com@, via @baikai-claude@. Takes an Anthropic API
key. The model is @claude-haiku-4-5-20251001@, with its published limits (200000
tokens of context, 8192 of output) as the 'Model' limits. For another Claude model
use 'anthropicCompatBackend' with the base URL @https:\/\/api.anthropic.com@.
-}
claudeBackend :: (MonadIO m) => Text -> IO (Backend m)
claudeBackend apiKey = do
  model <- anthropicModel "https://api.anthropic.com" "claude-haiku-4-5-20251001"
  let model' = model {contextWindow = 200000, maxOutputTokens = 8192}
      opts = emptyOptions {apiKey = Just (ApiKeyLiteral apiKey), maxTokens = Just 4096}
  pure (baikaiBackend model' opts)

{- | Any host that speaks the Anthropic Messages API, via @baikai-claude@. Takes the
base URL, the model id and the API key.

Unlike the key of 'openAICompatBackend', this one is not a 'Maybe': a hosted
Anthropic-compatible host checks a real key, and a local proxy that does not still
needs a placeholder, which only the caller can choose. The key is passed on as it is.
-}
anthropicCompatBackend :: (MonadIO m) => Text -> Text -> Text -> IO (Backend m)
anthropicCompatBackend baseUrl modelId apiKey = do
  model <- anthropicModel baseUrl modelId
  let model' = model {contextWindow = 8192, maxOutputTokens = 4096}
      opts = emptyOptions {apiKey = Just (ApiKeyLiteral apiKey), maxTokens = Just 4096}
  pure (baikaiBackend model' opts)

{- | A local Ollama server, through its OpenAI-compatible Chat Completions API, via
@baikai-openai@. Like 'openAICompatBackend' without an API key, for the server at
@OLLAMA_HOST@, read through 'normaliseOllamaHost'; without @OLLAMA_HOST@ it is
@http:\/\/127.0.0.1:11434@.
-}
localOllamaBackend :: (MonadIO m) => Text -> IO (Backend m)
localOllamaBackend modelId = do
  mHost <- lookupEnv "OLLAMA_HOST"
  let baseUrl = maybe "http://127.0.0.1:11434" (normaliseOllamaHost . T.pack) mHost
  openAICompatBackend baseUrl modelId Nothing

{- | Add a scheme and a default port to an @OLLAMA_HOST@-style value that lacks them.

Ollama's convention is a bare @host@ or @host:port@. A value without
@\"scheme:\/\/\"@ gets @http:\/\/@ in front, and @:11434@ behind when it names no
port. A value with a scheme is returned as it is, port included, so that
@https:\/\/tunnel-host@ stays on port 443.

>>> normaliseOllamaHost "gpu-box"
"http://gpu-box:11434"
>>> normaliseOllamaHost "gpu-box:8080"
"http://gpu-box:8080"
>>> normaliseOllamaHost "https://tunnel-host"
"https://tunnel-host"
-}
normaliseOllamaHost :: Text -> Text
normaliseOllamaHost h
  | "://" `T.isInfixOf` h = h
  | ":" `T.isInfixOf` h = "http://" <> h
  | otherwise = "http://" <> h <> ":11434"

{- | Any host that speaks the OpenAI Chat Completions API, hosted or self-hosted (vLLM,
the llama.cpp server), via @baikai-openai@. Takes the base URL, the model id and
the API key.

The key is a 'Maybe': @Nothing@ for a host that checks no credential, such as a bare
Ollama or llama.cpp server, and @Just@ the key for a host that does.

The token cap is sent as @max_tokens@, not as @max_completion_tokens@, which
@baikai-openai@ sends by default and Ollama ignores. The host's other auto-detected
quirks are kept. @api.openai.com@ itself keeps @max_completion_tokens@, because its
newer models reject @max_tokens@.
-}
openAICompatBackend :: (MonadIO m) => Text -> Text -> Maybe Text -> IO (Backend m)
openAICompatBackend baseUrl modelId mApiKey = do
  OpenAIProvider.register
  let model =
        (mkModel OpenAIChatCompletions modelId baseUrl)
          { contextWindow = 8192
          , maxOutputTokens = 4096
          , compat = legacyMaxTokensCompat baseUrl
          }
      opts =
        emptyOptions
          { apiKey = Just (maybe (ApiKeyLiteral "unused") ApiKeyLiteral mApiKey)
          , maxTokens = Just 4096
          }
  pure (baikaiBackend model opts)

{- | The 'Compat' for 'openAICompatBackend': the one @baikai-openai@ auto-detects from
the base URL, with the token cap named @max_tokens@. @api.openai.com@ is left to
auto-detection ('CompatNone').
-}
legacyMaxTokensCompat :: Text -> Compat
legacyMaxTokensCompat baseUrl
  | maybe False (`hostMatchesSuffix` "api.openai.com") (urlHost baseUrl) = CompatNone
  | otherwise =
      CompatOpenAICompletions
        (autoDetectOpenAICompletions baseUrl) {maxTokensField = MaxTokensField}
