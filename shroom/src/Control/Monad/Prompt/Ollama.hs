{-# LANGUAGE OverloadedRecordDot #-}

module Control.Monad.Prompt.Ollama (module Control.Monad.Prompt.Ollama) where

-- base
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (fromMaybe)

-- text

import Data.Text (Text, pack)
import Data.Text qualified as T
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
  assistantMessage,
  chat,
  defaultChatOps,
  systemMessage,
  toolMessage,
  userMessage,
 )
import Data.Ollama.Common.Config (OllamaConfig (..), defaultOllamaConfig)
import Data.Ollama.Common.Error (OllamaError (..))
import Data.Ollama.Common.SchemaBuilder (JsonType (..), Property (..), Schema (..))
import Data.Ollama.Common.Types (
  ChatResponse (..),
  FunctionDef (..),
  FunctionParameters (..),
  InputTool (..),
  Message (..),
  OutputFunction (..),
  ToolCall (..),
 )

-- shroom
import Control.Monad.Prompt (ContextItem (..), LLMBackend (..), PromptConfig (..))
import Control.Monad.Prompt.Schema (ToolDef (..), inlineSchema)
import Control.Monad.Prompt.Tool (ToolLoopOps (..), genericToolLoop)

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

{- | Convert a list of 'ContextItem' values to Ollama messages, appending
the type description as a final user turn.
-}
contextItemsToOllama :: [ContextItem] -> Text -> NonEmpty Message
contextItemsToOllama items typeDesc =
  let toMsg (SystemMessage t) = systemMessage t
      toMsg (UserMessage t) = userMessage t
      toMsg (AssistantMessage t) = assistantMessage t
      allMsgs = fmap toMsg items <> [userMessage typeDesc]
   in case allMsgs of
        [] -> userMessage typeDesc :| []
        (x : xs) -> x :| xs

{- | Convert a 'ToolDef' to an Ollama 'InputTool'.
Extracts properties and required fields from the JSON schema.

The schema is inlined first: 'FunctionParameters' has no field for @$ref@ or
@$defs@ at all, and 'Control.Monad.Prompt.Schema.schemaWithDefs' always
@$ref@s its own root, so passing it un-inlined loses every property
(confirmed directly in @cabal repl@) regardless of what the Ollama server
does with @$ref@ on the wire.
-}
toOllamaTool :: ToolDef -> InputTool
toOllamaTool td =
  InputTool
    { toolType = "function"
    , function =
        FunctionDef
          { functionName = td.toolDefName
          , functionDescription = Just td.toolDefDescription
          , functionParameters = Just (schemaToFunctionParameters (inlineSchema td.toolDefSchema))
          , functionStrict = Nothing
          }
    }

-- | Convert an inlined OpenAPI schema 'Value' to 'FunctionParameters'.
schemaToFunctionParameters :: Value -> FunctionParameters
schemaToFunctionParameters (Object km) =
  FunctionParameters
    { parameterType = "object"
    , parameterProperties = case KM.lookup "properties" km of
        Just (Object ps) ->
          Just $
            Map.fromList
              [ (Key.toText k, schemaToFunctionParameters v)
              | (k, v) <- KM.toList ps
              ]
        _ -> Nothing
    , requiredParams = case KM.lookup "required" km of
        Just (Array arr) -> Just [t | String t <- toList arr]
        _ -> Nothing
    , additionalProperties = Nothing
    }
schemaToFunctionParameters _ =
  FunctionParameters
    { parameterType = "string"
    , parameterProperties = Nothing
    , requiredParams = Nothing
    , additionalProperties = Nothing
    }

-- | Build 'ToolLoopOps' for the Ollama backend.
ollamaToolLoopOps ::
  (MonadIO m) =>
  OllamaBackendConfig ->
  -- | output schema (for final structured call)
  Value ->
  [ToolDef] ->
  PromptConfig ->
  ToolLoopOps m (NonEmpty Message) ChatResponse
ollamaToolLoopOps cfg schema toolDefs promptCfg =
  let (fmt, unwrap) = schemaToFormatAndUnwrap schema
   in ToolLoopOps
        { callModel = \msgs -> liftIO $ do
            let ops =
                  defaultChatOps
                    { modelName = cfg.ollamaModel
                    , messages = msgs
                    , tools = Just (fmap toOllamaTool toolDefs)
                    , format = Just fmt
                    , stream = Nothing
                    }
            result <- try @SomeException $ chat ops (Just cfg.ollamaConfig)
            pure $ case result of
              Left ex -> Left ("IO error: " <> pack (show ex))
              Right (Left err) -> Left (renderOllamaError err)
              Right (Right resp) -> Right resp
        , callModelNoTools = \msgs -> liftIO $ do
            let ops =
                  defaultChatOps
                    { modelName = cfg.ollamaModel
                    , messages = msgs
                    , tools = Nothing
                    , format = Just fmt
                    , stream = Nothing
                    }
            result <- try @SomeException $ chat ops (Just cfg.ollamaConfig)
            pure $ case result of
              Left ex -> Left ("IO error: " <> pack (show ex))
              Right (Left err) -> Left (renderOllamaError err)
              Right (Right resp) -> Right resp
        , detectTools = \resp ->
            case resp.message of
              Nothing -> Nothing
              Just msg -> case msg.tool_calls of
                Nothing -> Nothing
                Just [] -> Nothing
                Just calls ->
                  let extracted =
                        ( \tc ->
                            ( tc.outputFunction.outputFunctionName
                            , tc.outputFunction.outputFunctionName
                            , Object
                                ( KM.fromList
                                    [ (Key.fromText k, v)
                                    | (k, v) <- Map.toList tc.outputFunction.arguments
                                    ]
                                )
                            )
                        )
                          <$> calls
                   in if null extracted then Nothing else Just extracted
        , appendExchange = \msgs resp tagged ->
            -- Ollama rejects messages with empty content; use a space when the
            -- assistant message has no text (e.g. pure tool-call turns).
            let ensureContent msg
                  | T.null msg.content = msg {content = " "}
                  | otherwise = msg
                assistantMsg = ensureContent $ fromMaybe (assistantMessage " ") resp.message
                toolMsgs =
                  [ toolMessage (either ("Error: " <>) id res)
                  | (_, _, res) <- tagged
                  ]
             in msgs <> (assistantMsg :| toolMsgs)
        , extractText = \resp -> unwrap (maybe "" (.content) resp.message)
        , logEvent = \msg -> liftIO $ maybe (pure ()) ($ msg) promptCfg.debugLog
        }

instance LLMBackend OllamaBackendConfig where
  runChatWithTools cfg promptCfg ctx typeDesc schema toolDefs dispatch maxToolSteps
    | null toolDefs = liftIO $ do
        let (fmt, unwrap) = schemaToFormatAndUnwrap schema
            msgs = contextItemsToOllama ctx typeDesc
            ops =
              defaultChatOps
                { modelName = cfg.ollamaModel
                , messages = msgs
                , format = Just fmt
                , stream = Nothing
                }
        result <- try @SomeException $ chat ops (Just cfg.ollamaConfig)
        pure $ case result of
          Left ex -> Left ("IO error: " <> pack (show ex))
          Right (Left err) -> Left (renderOllamaError err)
          Right (Right resp) -> case resp.message of
            Nothing -> Left "Ollama returned a response with no message"
            Just msg -> Right (unwrap msg.content)
    | otherwise = do
        let msgs = contextItemsToOllama ctx typeDesc
            ops = ollamaToolLoopOps cfg schema toolDefs promptCfg
        genericToolLoop ops dispatch msgs maxToolSteps

{- | Build an Ollama 'Format' from an OpenAPI schema 'Value'.
Returns @(format, unwrapFn)@ where @unwrapFn@ strips the @{\"result\":...}@
wrapper that is added for non-object schemas (scalars, arrays).

The schema is inlined via 'inlineSchema' first. This is not because Ollama's
server fails to resolve @$ref@ on the wire — a live test against Ollama
0.30.6 showed it does, on both this field and a tool's parameters — but
because this module's own 'Format' \/ 'Data.Ollama.Common.SchemaBuilder.Schema'
have no way to represent @$ref@ or @$defs@ at all. Since
'Control.Monad.Prompt.Schema.schemaWithDefs' always @$ref@s its own root,
skipping the inline step turns /every/ record type's schema into an
unconstrained 'JsonFormat' here, not just ones with nested records
(confirmed directly in @cabal repl@).
-}
schemaToFormatAndUnwrap :: Value -> (Format, Text -> Text)
schemaToFormatAndUnwrap schema = schemaToFormatAndUnwrap' (inlineSchema schema)

schemaToFormatAndUnwrap' :: Value -> (Format, Text -> Text)
schemaToFormatAndUnwrap' (Object km) =
  case KM.lookup "type" km of
    Just (String "object") ->
      (SchemaFormat (openApiObjectToSchema km), id)
    Just (String typeStr) ->
      let jtype = textToJsonType typeStr (KM.lookup "items" km)
          wrapped = Schema (Map.singleton "result" (Property jtype)) ["result"]
       in (SchemaFormat wrapped, unwrapResult)
    _ -> (JsonFormat, id)
schemaToFormatAndUnwrap' _ = (JsonFormat, id)

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
