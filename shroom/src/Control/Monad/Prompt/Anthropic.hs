{-# LANGUAGE OverloadedRecordDot #-}

{- | Anthropic Claude backend for 'PromptT'.

Import this module alongside "Control.Monad.Prompt" to use the Claude API:

@
import Control.Monad.Prompt
import Control.Monad.Prompt.Anthropic

cfg <- pure $ mkAnthropicConfig "sk-ant-..."
result <- runPromptResultTWith cfg $ runPromptT defaultPromptConfig $ do
  context "The user's name is Alice."
  prompt \@User
@
-}
module Control.Monad.Prompt.Anthropic (module Control.Monad.Prompt.Anthropic) where

-- base
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (toList)
import Data.Maybe (mapMaybe)
import Numeric.Natural (Natural)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- vector
import Data.Vector qualified as V

-- aeson
import Data.Aeson (Value)

-- claude
import Claude.V1
import Claude.V1.Messages

-- shroom
import Control.Monad.Prompt (ContextItem (..), LLMBackend (..), PromptConfig (..))
import Control.Monad.Prompt.Schema (ToolDef (..), fixSchemaForAnthropic, inlineSchema)
import Control.Monad.Prompt.Tool (ToolLoopOps (..), genericToolLoop)

-- | Configuration for the Anthropic Claude API.
data AnthropicConfig = AnthropicConfig
  { apiKey :: Text
  -- ^ Your Anthropic API key.
  , model :: Text
  -- ^ Model identifier, e.g. @\"claude-haiku-4-5-20251001\"@.
  , maxTokens :: Natural
  -- ^ Maximum number of tokens in the model response. Default: 4096.
  }

{- | Make a configuration for the Anthropic backend.

Provide a specified API key,
reasonable defaults for other parameters: Claude Haiku 4-5-20251001 and max tokens 4096.
-}
mkAnthropicConfig ::
  -- | Your Anthropic API key.
  Text ->
  AnthropicConfig
mkAnthropicConfig apiKey =
  AnthropicConfig
    { apiKey = apiKey
    , model = "claude-haiku-4-5-20251001"
    , maxTokens = 4096
    }

{- | Convert a list of 'ContextItem' values to Anthropic API messages.
'SystemMessage' items are collected into the @system@ field;
'UserMessage' and 'AssistantMessage' items become @messages@.
A final user message containing @typeDesc@ is always appended.
-}
contextItemsToAnthropic :: [ContextItem] -> Text -> (Maybe SystemPrompt, [Message])
contextItemsToAnthropic items typeDesc =
  let systemTexts = [t | SystemMessage t <- items]
      mSystem = case systemTexts of
        [] -> Nothing
        ts -> Just (systemText (T.intercalate "\n" ts))
      chatItems = [item | item <- items, not (isSystem item)]
      toMsg (UserMessage t) = Just Message {role = User, content = [Content_Text {text = t, cache_control = Nothing}], cache_control = Nothing}
      toMsg (AssistantMessage t) = Just Message {role = Assistant, content = [Content_Text {text = t, cache_control = Nothing}], cache_control = Nothing}
      toMsg (SystemMessage _) = Nothing
      chatMsgs = mapMaybe toMsg chatItems
      -- Append the type description as the final user turn
      finalMsg = Message {role = User, content = [Content_Text {text = typeDesc, cache_control = Nothing}], cache_control = Nothing}
   in (mSystem, chatMsgs <> [finalMsg])
  where
    isSystem (SystemMessage _) = True
    isSystem _ = False

-- | Convert a 'ToolDef' to an Anthropic 'ToolDefinition'.
toAnthropicTool :: ToolDef -> ToolDefinition
toAnthropicTool td =
  inlineTool $ strictFunctionTool td.toolDefName (Just td.toolDefDescription) (inlineSchema td.toolDefSchema)

-- | Build 'ToolLoopOps' for the Anthropic backend.
anthropicToolLoopOps ::
  (MonadIO m) =>
  Methods ->
  AnthropicConfig ->
  Maybe SystemPrompt ->
  -- | output schema (for final structured call)
  Value ->
  Maybe (V.Vector ToolDefinition) ->
  PromptConfig ->
  ToolLoopOps m [Message] MessageResponse
anthropicToolLoopOps methods cfg mSystem schema mTools promptCfg =
  ToolLoopOps
    { callModel = \msgs -> do
        result <-
          liftIO $
            try @SomeException $
              methods.createMessage
                _CreateMessage
                  { model = cfg.model
                  , messages = V.fromList msgs
                  , system = mSystem
                  , max_tokens = cfg.maxTokens
                  , tools = mTools
                  , -- Only request structured output when there are no tools active,
                    -- or Anthropic rejects the combination when tool_use stop_reason is expected.
                    -- We set output_config on every call; if the model stops for tool_use
                    -- the schema is ignored; if it stops for end_turn we get JSON.
                    output_config = Just (jsonSchemaConfig (fixSchemaForAnthropic schema))
                  }
        pure $ case result of
          Left ex -> Left ("HTTP error: " <> pack (show ex))
          Right resp -> Right resp
    , callModelNoTools = \msgs -> do
        result <-
          liftIO $
            try @SomeException $
              methods.createMessage
                _CreateMessage
                  { model = cfg.model
                  , messages = V.fromList msgs
                  , system = mSystem
                  , max_tokens = cfg.maxTokens
                  , tools = Nothing
                  , output_config = Just (jsonSchemaConfig (fixSchemaForAnthropic schema))
                  }
        pure $ case result of
          Left ex -> Left ("HTTP error: " <> pack (show ex))
          Right resp -> Right resp
    , detectTools = \resp ->
        case resp.stop_reason of
          Just Tool_Use ->
            let calls =
                  [ (cb.id, cb.name, cb.input)
                  | cb <- toList resp.content
                  , ContentBlock_Tool_Use {} <- [cb]
                  ]
             in if null calls then Nothing else Just calls
          _ -> Nothing
    , appendExchange = \msgs resp tagged ->
        let assistantContents = V.fromList $ mapMaybe contentBlockToContent (toList resp.content)
            assistantMsg = Message {role = Assistant, content = assistantContents, cache_control = Nothing}
            toolResultContents =
              [ Content_Tool_Result
                  { tool_use_id = uid
                  , content = Just (either ("Error: " <>) Prelude.id res)
                  , is_error = either (const (Just True)) (const Nothing) res
                  }
              | (uid, _name, res) <- tagged
              ]
            -- If any tool call failed, append an extra reminder so the model
            -- records the failure in the final structured-output response.
            failedNames = [name | (_uid, name, Left _) <- tagged]
            extraReminder
              | null failedNames = []
              | otherwise =
                  [ Content_Text
                      { text =
                          "Note: the following tool(s) returned errors: "
                            <> T.intercalate ", " failedNames
                            <> ". Make sure to record these as non-null error strings in your final JSON response."
                      , cache_control = Nothing
                      }
                  ]
            toolMsg = Message {role = User, content = V.fromList (toolResultContents <> extraReminder), cache_control = Nothing}
         in msgs <> [assistantMsg, toolMsg]
    , extractText = \resp ->
        mconcat
          [ t
          | ContentBlock_Text {text = t} <- toList resp.content
          ]
    , logEvent = \msg -> liftIO $ maybe (pure ()) ($ msg) promptCfg.debugLog
    }

instance LLMBackend AnthropicConfig where
  runChatWithTools cfg promptCfg ctx typeDesc schema toolDefs dispatch maxToolSteps = do
    clientEnv <- liftIO $ getClientEnv "https://api.anthropic.com"
    let methods = makeMethods clientEnv cfg.apiKey (Just "2023-06-01")
        (mSystem, msgs) = contextItemsToAnthropic ctx typeDesc
        mTools = case toolDefs of
          [] -> Nothing
          ts -> Just (V.fromList (fmap toAnthropicTool ts))
        ops = anthropicToolLoopOps methods cfg mSystem schema mTools promptCfg
    genericToolLoop ops dispatch msgs maxToolSteps
