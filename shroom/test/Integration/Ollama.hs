module Main (main) where

-- base
import Data.IORef
import System.Environment (lookupEnv)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCaseSteps, (@?=))

-- sop-core
import Data.SOP (NP (..))

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Ollama
import Control.Monad.Prompt.Tool (ToolHandler (..))
import Control.Monad.Prompt.Tool.Web (duckDuckGoSearchHandler, webFetchHandler)

-- test
import CelebrityTypes
import ConferenceTypes
import Types

main :: IO ()
main = do
  mHost <- lookupEnv "OLLAMA_HOST"
  mModel <- lookupEnv "OLLAMA_MODEL"
  let host = pack <$> mHost
      model = maybe "llama3.2:3b" pack mModel
  defaultMain $ ollamaIntegrationTests host model

ollamaIntegrationTests :: Maybe Text -> Text -> TestTree
ollamaIntegrationTests mHost model =
  testGroup
    "PromptT Ollama integration"
    [ testCaseSteps "prompt returns a User" $ \step -> do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            ollamaPromptConfig =
              defaultPromptConfig
                { maxRetries = 10
                , debugLog = Just (step . T.unpack)
                }
        result <- runPromptResultTWith cfg $ runPromptTNoTools ollamaPromptConfig $ do
          context "Return a JSON object for a user named Alice with email alice@example.com"
          prompt @User
        case result of
          Left err -> assertFailure (show err)
          Right user -> userName user @?= "Alice"
    , testCaseSteps "prompt returns a Counter" $ \step -> do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            ollamaPromptConfig =
              defaultPromptConfig
                { maxRetries = 10
                , debugLog = Just (step . T.unpack)
                }
        result <- runPromptResultTWith cfg $ runPromptTNoTools ollamaPromptConfig $ do
          context "Return a JSON counter object with value 42."
          prompt @Counter
        case result of
          Left err -> assertFailure (show err)
          Right (Counter n) -> n @?= 42
    , testCaseSteps "conferenceChain runs end-to-end" $ \step -> do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            ollamaPromptConfig =
              defaultPromptConfig
                { maxRetries = 10
                , debugLog = Just (step . T.unpack)
                }
        result <- runPromptResultTWith cfg $ runPromptTNoTools ollamaPromptConfig conferenceChain
        case result of
          Left err -> assertFailure (show err)
          Right (allSpeakers, talks, schedule) -> do
            let n = length (speakers allSpeakers)
            (n >= 3 && n <= 10) @?= True
            length talks @?= n
            not (null (scheduleSlots schedule)) @?= True
            let slots = scheduleSlots schedule
                pairs = zip slots (drop 1 slots)
            all (\(a, b) -> slotEnd a == slotStart b) pairs @?= True
    , testCaseSteps "celebrityChain: chain completes successfully with tools available" $ \step -> do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            pcfg =
              defaultPromptConfig
                { maxRetries = 5
                , maxToolSteps = Just 10
                , debugLog = Just (step . T.unpack)
                }
        let handlers = duckDuckGoSearchHandler :* webFetchHandler :* Nil
        result <- runPromptResultTWith cfg $ runPromptT pcfg handlers celebrityChain
        case result of
          Left err -> assertFailure (show err)
          Right fact -> do
            assertBool "celebrity not empty" (not (T.null (celebrity fact)))
            assertBool "trivia fact not empty" (not (T.null (triviaFact fact)))
    , testCaseSteps "conferenceChainWithTools: web_search tool is invoked at least once" $ \step -> do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            pcfg =
              defaultPromptConfig
                { maxRetries = 10
                , maxToolSteps = Just 10
                , debugLog = Just (step . T.unpack)
                }
        callCount <- newIORef (0 :: Int)
        let countingHandler = ToolHandler $ \q -> do
              atomicModifyIORef' callCount (\n -> (n + 1, ()))
              runToolHandler duckDuckGoSearchHandler q
            handlers = countingHandler :* Nil
        result <-
          runPromptResultTWith cfg $
            runPromptT pcfg handlers conferenceChainWithTools
        case result of
          Left err -> assertFailure (show err)
          Right (allSpeakers, talks, _schedule) -> do
            let n = length (speakers allSpeakers)
            assertBool "at least 3 speakers" (n >= 3)
            length talks @?= n
        n <- readIORef callCount
        assertBool "web_search tool was called at least once" (n > 0)
    ]
