module Main (main) where

-- base
import System.Environment (lookupEnv)

-- text
import Data.Text (Text, pack)

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Ollama

-- test

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
    [ testCase "prompt returns a User" $ do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            ollamaPromptConfig = defaultPromptConfig {maxRetries = 10}
        result <- runPromptResultTWith cfg $ runPromptT ollamaPromptConfig $ do
          context "Return a JSON object for a user named Alice with email alice@example.com"
          prompt @User
        case result of
          Left err -> assertFailure (show err)
          Right user -> userName user @?= "Alice"
    , testCase "prompt returns a Counter" $ do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            ollamaPromptConfig = defaultPromptConfig {maxRetries = 10}
        result <- runPromptResultTWith cfg $ runPromptT ollamaPromptConfig $ do
          context "Return a JSON counter object with value 42."
          prompt @Counter
        case result of
          Left err -> assertFailure (show err)
          Right (Counter n) -> n @?= 42
    , testCase "conferenceChain runs end-to-end" $ do
        let cfg = (defaultOllamaBackendConfig mHost) {ollamaModel = model}
            ollamaPromptConfig = defaultPromptConfig {maxRetries = 10}
        result <- runPromptResultTWith cfg $ runPromptT ollamaPromptConfig conferenceChain
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
    ]
