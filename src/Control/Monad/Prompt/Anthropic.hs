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

-- witherable
import Witherable ((<&?>))

-- claude
import Claude.V1
import Claude.V1.Messages

-- shroom
import Control.Monad.Prompt (
  ContextItem (..),
  LLMBackend (..),
  fixSchemaForAnthropic,
 )

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

instance LLMBackend AnthropicConfig where
  runChat cfg ctx typeDesc schema = liftIO $ do
    result <- try @SomeException $ do
      clientEnv <- getClientEnv "https://api.anthropic.com"
      let methods = makeMethods clientEnv cfg.apiKey (Just "2023-06-01")
          (mSystem, msgs) = contextItemsToAnthropic ctx typeDesc
      resp <-
        methods.createMessage
          _CreateMessage
            { model = cfg.model
            , messages = V.fromList msgs
            , system = mSystem
            , max_tokens = cfg.maxTokens
            , output_config = Just (jsonSchemaConfig (fixSchemaForAnthropic schema))
            }
      let MessageResponse {content} = resp
          texts =
            toList $
              content <&?> \case
                ContentBlock_Text {text = t} -> Just t
                _ -> Nothing
      pure $ mconcat texts
    pure $ case result of
      Left ex -> Left ("HTTP error: " <> pack (show ex))
      Right txt -> Right txt
