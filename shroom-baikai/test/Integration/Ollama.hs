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

-- shroom
import Control.Monad.Prompt.Baikai (localOllamaBackend)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.Tool (ToolHandler (..), runTool, runToolHandler, toolBinding)
import Control.Monad.Prompt.Tool.Web (DuckDuckGoSearch, WebFetch, duckDuckGoSearchHandler, webFetchHandler)

-- test
import CelebrityTypes
import ConferenceTypes
import Types

main :: IO ()
main = do
  mModel <- lookupEnv "OLLAMA_MODEL"
  let model = maybe "llama3.2:3b" pack mModel
  ollamaIntegrationTests model >>= defaultMain

{- | 'localOllamaBackend' honours @OLLAMA_HOST@ itself, so this suite no
longer reads it directly the way it read it to build 'defaultOllamaBackendConfig'.
-}
ollamaIntegrationTests :: Text -> IO TestTree
ollamaIntegrationTests model = do
  backend <- localOllamaBackend model
  pure $
    testGroup
      "Ollama integration"
      [ testCaseSteps "prompt returns a User" $ \step -> do
          let ollamaPromptConfig =
                defaultPromptConfig
                  { maxRetries = 10
                  , debugLog = Just (step . T.unpack)
                  }
          result <- Effect.runPromptResultEff backend ollamaPromptConfig $ do
            Effect.context "Return a JSON object for a user named Alice with email alice@example.com"
            Effect.prompt @User
          case result of
            Left err -> assertFailure (show err)
            Right user -> userName user @?= "Alice"
      , testCaseSteps "prompt returns a Counter" $ \step -> do
          let ollamaPromptConfig =
                defaultPromptConfig
                  { maxRetries = 10
                  , debugLog = Just (step . T.unpack)
                  }
          result <- Effect.runPromptResultEff backend ollamaPromptConfig $ do
            Effect.context "Return a JSON counter object with value 42."
            Effect.prompt @Counter
          case result of
            Left err -> assertFailure (show err)
            Right (Counter n) -> n @?= 42
      , testCaseSteps "conferenceChain runs end-to-end" $ \step -> do
          let ollamaPromptConfig =
                defaultPromptConfig
                  { maxRetries = 10
                  , debugLog = Just (step . T.unpack)
                  }
          result <- Effect.runPromptResultEff backend ollamaPromptConfig conferenceChain
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
          let pcfg =
                defaultPromptConfig
                  { maxRetries = 5
                  , maxToolSteps = Just 10
                  , debugLog = Just (step . T.unpack)
                  }
          result <-
            Effect.runPromptResultEff backend pcfg $
              runTool duckDuckGoSearchHandler $
                runTool webFetchHandler $
                  celebrityChain [toolBinding @DuckDuckGoSearch, toolBinding @WebFetch]
          case result of
            Left err -> assertFailure (show err)
            Right fact -> do
              assertBool "celebrity not empty" (not (T.null (celebrity fact)))
              assertBool "trivia fact not empty" (not (T.null (triviaFact fact)))
      , testCaseSteps "conferenceChainWithTools: web_search tool is invoked at least once" $ \step -> do
          let pcfg =
                defaultPromptConfig
                  { maxRetries = 10
                  , maxToolSteps = Just 10
                  , debugLog = Just (step . T.unpack)
                  }
          callCount <- newIORef (0 :: Int)
          let countingHandler = ToolHandler $ \q -> do
                atomicModifyIORef' callCount (\n -> (n + 1, ()))
                runToolHandler duckDuckGoSearchHandler q
          result <-
            Effect.runPromptResultEff backend pcfg $
              runTool countingHandler (conferenceChainWithTools [toolBinding @DuckDuckGoSearch])
          case result of
            Left err -> assertFailure (show err)
            Right (allSpeakers, talks, _schedule) -> do
              let n = length (speakers allSpeakers)
              assertBool "at least 3 speakers" (n >= 3)
              length talks @?= n
          n <- readIORef callCount
          assertBool "web_search tool was called at least once" (n > 0)
      ]
