module Control.Monad.Prompt.Ollama (module Control.Monad.Prompt.Ollama) where

-- base
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))

-- text
import Data.Text (Text, pack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- aeson
import Data.Aeson (Value (..), encode)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM

-- containers
import Data.Map.Strict qualified as Map

-- ollama-haskell
import Data.Ollama.Chat (
  ChatOps (..),
  Format (..),
  chat,
  defaultChatOps,
  systemMessage,
  userMessage,
 )
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.SchemaBuilder (JsonType (..), Property (..), Schema (..))
import Data.Ollama.Common.Types (ChatResponse (..), Message (..))

-- shroom
import Control.Monad.Prompt (LLMBackend (..))

-- | Configuration for an Ollama-hosted local model.
data OllamaBackendConfig = OllamaBackendConfig
  { ollamaConfig :: OllamaConfig
  -- ^ Connection settings (host URL, timeout, retries).
  , ollamaModel :: Text
  -- ^ Model name to use, e.g. @\"llama3.2:3b\"@.
  }

{- | Default Ollama config pointing at @http://127.0.0.1:11434@ with the
@\"llama3.2:3b\"@ model.

Pass @Just url@ to override the host URL, e.g.
@defaultOllamaBackendConfig (Just \"http://myserver:11434\")@.
-}
defaultOllamaBackendConfig :: Maybe Text -> OllamaBackendConfig
defaultOllamaBackendConfig mHost =
  OllamaBackendConfig
    { ollamaConfig = maybe defaultOllamaConfig (\h -> defaultOllamaConfig {hostUrl = h}) mHost
    , ollamaModel = "llama3.2:3b"
    }

instance LLMBackend OllamaBackendConfig where
  runChat cfg ctx typeDesc schema = liftIO $ do
    let (fmt, unwrap) = schemaToFormatAndUnwrap schema
        msgs = case ctx of
          "" -> userMessage typeDesc :| []
          _ -> systemMessage ctx :| [userMessage typeDesc]
        ops =
          defaultChatOps
            { modelName = cfg.ollamaModel
            , messages = msgs
            , format = Just fmt
            , stream = Nothing
            }
    result <- try @SomeException $ chat ops (Just cfg.ollamaConfig)
    case result of
      Left ex -> pure $ Left ("IO error: " <> pack (show ex))
      Right (Left err) -> pure $ Left (renderOllamaError err)
      Right (Right resp) -> case resp.message of
        Nothing -> pure $ Left "Ollama returned a response with no message"
        Just msg -> pure $ Right (unwrap msg.content)

{- | Build an Ollama 'Format' from an OpenAPI schema 'Value'.
Returns @(format, unwrapFn)@ where @unwrapFn@ strips the @{\"result\":...}@
wrapper that is added for non-object schemas (scalars, arrays).
-}
schemaToFormatAndUnwrap :: Value -> (Format, Text -> Text)
schemaToFormatAndUnwrap (Object km) =
  case KM.lookup "type" km of
    Just (String "object") ->
      (SchemaFormat (openApiObjectToSchema km), id)
    Just (String typeStr) ->
      let jtype = textToJsonType typeStr (KM.lookup "items" km)
          wrapped = Schema (Map.singleton "result" (Property jtype)) ["result"]
       in (SchemaFormat wrapped, unwrapResult)
    _ -> (JsonFormat, id)
schemaToFormatAndUnwrap _ = (JsonFormat, id)

{- | Extract the value of the @\"result\"@ key from a JSON object response.
Used to unwrap scalars/arrays that were wrapped in @{\"result\": ...}@.
-}
unwrapResult :: Text -> Text
unwrapResult t = case Aeson.decodeStrictText t of
  Just (Object km) -> case KM.lookup "result" km of
    Just v -> toStrict (decodeUtf8 (encode v))
    Nothing -> t
  _ -> t

-- | Convert an OpenAPI object schema 'KeyMap' to an ollama-haskell 'Schema'.
openApiObjectToSchema :: KM.KeyMap Value -> Schema
openApiObjectToSchema km =
  let props = case KM.lookup "properties" km of
        Just (Object ps) ->
          Map.fromList
            [ (Key.toText k, Property (valueToJsonType v))
            | (k, v) <- KM.toList ps
            ]
        _ -> Map.empty
      req = case KM.lookup "required" km of
        Just (Array arr) -> [t | String t <- toList arr]
        _ -> []
   in Schema props req

-- | Convert an OpenAPI type string (and optional items schema) to a 'JsonType'.
textToJsonType :: Text -> Maybe Value -> JsonType
textToJsonType "string" _ = JString
textToJsonType "number" _ = JNumber
textToJsonType "integer" _ = JInteger
textToJsonType "boolean" _ = JBoolean
textToJsonType "null" _ = JNull
textToJsonType "array" items = JArray (maybe JString valueToJsonType items)
textToJsonType "object" _ = JObject (Schema Map.empty [])
textToJsonType _ _ = JString

-- | Convert an OpenAPI property schema 'Value' to a 'JsonType'.
valueToJsonType :: Value -> JsonType
valueToJsonType (Object o) = case KM.lookup "type" o of
  Just (String "object") -> JObject (openApiObjectToSchema o)
  Just (String t) -> textToJsonType t (KM.lookup "items" o)
  _ -> JString
valueToJsonType _ = JString

renderOllamaError :: OllamaError -> Text
renderOllamaError = \case
  HttpError e -> "Ollama HTTP error: " <> pack (show e)
  DecodeError msg v -> "Ollama decode error: " <> pack msg <> " (value: " <> pack v <> ")"
  ApiError msg -> "Ollama API error: " <> msg
  FileError e -> "Ollama file error: " <> pack (show e)
  JsonSchemaError e -> "Ollama JSON schema error: " <> pack e
  TimeoutError e -> "Ollama timeout: " <> pack e
  InvalidRequest e -> "Ollama invalid request: " <> pack e
