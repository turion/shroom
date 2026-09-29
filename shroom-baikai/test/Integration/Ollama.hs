module Main (main) where

-- base
import Control.Monad.IO.Class (MonadIO)
import System.Environment (lookupEnv, setEnv)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, testCaseSteps, (@?=))

-- baikai
import Baikai.Api (Api (OpenAIChatCompletions))
import Baikai.Auth (ApiKeySource (ApiKeyEnv))
import Baikai.Model (Model (..), mkModel)
import Baikai.Options (Options (..), emptyOptions)

-- shroom
import Control.Monad.Prompt.Backend (Backend)
import Control.Monad.Prompt.Baikai (baikaiBackend, localOllamaBackend, normaliseOllamaHost)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.Tool (runTool, toolBinding)
import Control.Monad.Prompt.Tool.Web (DuckDuckGoSearch, WebFetch, duckDuckGoSearchHandler, webFetchHandler)

-- test
import CelebrityTypes
import Integration.Cases (backendIntegrationTests)
import ScalarTypes (ScalarInt (..), ScalarIntList (..), ScalarText (..))

main :: IO ()
main = do
  mModel <- lookupEnv "OLLAMA_MODEL"
  let model = maybe "qwen3:8b" pack mModel
  ollamaIntegrationTests model >>= defaultMain

{- | 'localOllamaBackend' honours @OLLAMA_HOST@ itself, so this suite no
longer reads it directly the way it read it to build 'defaultOllamaBackendConfig'.

The second backend built here, 'genericOpenAICompatBackend', is not one of
'Control.Monad.Prompt.Baikai'\'s three front-door constructors: it is the
same @baikai-openai@ transport 'localOllamaBackend' itself uses, built
directly via 'baikaiBackend' \/ 'Baikai.Model.mkModel' \/ 'Baikai.Options.Options'
so that its 'Baikai.Options.apiKey' can be an 'ApiKeyEnv' rather than
'localOllamaBackend'\'s own 'Baikai.Auth.ApiKeyLiteral' placeholder — the
base-URL-plus-key-source resolution path a caller reaching for the fully
generic OpenAI-compatible constructor (rather than the local-Ollama
convenience one) would actually exercise. Ollama itself checks no
credential, so the env var it names only has to be non-empty, never
correct.
-}
ollamaIntegrationTests :: Text -> IO TestTree
ollamaIntegrationTests model = do
  localBackend <- localOllamaBackend model
  genericBackend <- genericOpenAICompatBackend model
  pure $
    testGroup
      "Ollama integration"
      [ testGroup
          "localOllamaBackend"
          [ backendIntegrationTests localBackend
          , testCaseSteps "celebrityChain: chain completes successfully with tools available" $ \step -> do
              let pcfg =
                    defaultPromptConfig
                      { maxRetries = 5
                      , maxToolSteps = Just 10
                      , debugLog = Just (step . T.unpack)
                      }
              result <-
                Effect.runPromptResultEff localBackend pcfg $
                  runTool duckDuckGoSearchHandler $
                    runTool webFetchHandler $
                      celebrityChain [toolBinding @DuckDuckGoSearch, toolBinding @WebFetch]
              case result of
                Left err -> assertFailure (show err)
                Right fact -> do
                  assertBool "celebrity not empty" (not (T.null (celebrity fact)))
                  assertBool "trivia fact not empty" (not (T.null (triviaFact fact)))
          , testCase "prompt returns a non-object Int" $ do
              result <- Effect.runPromptResultEff localBackend (defaultPromptConfig {maxRetries = 10}) $ do
                Effect.context "How many legs does a spider have? Answer with just the number."
                Effect.prompt @ScalarInt
              case result of
                Left err -> assertFailure (show err)
                Right (ScalarInt n) -> n @?= 8
          , testCase "prompt returns a non-object Text" $ do
              result <- Effect.runPromptResultEff localBackend (defaultPromptConfig {maxRetries = 10}) $ do
                Effect.context "Return the word \"hello\" and nothing else."
                Effect.prompt @ScalarText
              case result of
                Left err -> assertFailure (show err)
                Right (ScalarText t) -> assertBool "non-empty" (not (T.null t))
          , testCase "prompt returns a non-object array of Int" $ do
              result <- Effect.runPromptResultEff localBackend (defaultPromptConfig {maxRetries = 10}) $ do
                Effect.context "Return the first three positive integers as a JSON array, in order."
                Effect.prompt @ScalarIntList
              case result of
                Left err -> assertFailure (show err)
                Right (ScalarIntList xs) -> xs @?= [1, 2, 3]
          ]
      , testGroup
          "openAICompatBackend (explicit base URL, ApiKeyEnv)"
          [backendIntegrationTests genericBackend]
      ]

genericOpenAICompatBackend :: (MonadIO m) => Text -> IO (Backend m)
genericOpenAICompatBackend modelId = do
  mHost <- lookupEnv "OLLAMA_HOST"
  let baseUrl = maybe "http://127.0.0.1:11434" (normaliseOllamaHost . T.pack) mHost
      apiKeyEnvVar = "SHROOM_TEST_OLLAMA_API_KEY"
  setEnv apiKeyEnvVar "unused-ollama-checks-no-credential"
  let model' =
        (mkModel OpenAIChatCompletions modelId baseUrl)
          { contextWindow = 8192
          , maxOutputTokens = 4096
          }
      opts =
        emptyOptions
          { apiKey = Just (ApiKeyEnv apiKeyEnvVar)
          , maxTokens = Just 4096
          }
  pure (baikaiBackend model' opts)
