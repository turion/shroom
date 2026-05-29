module Main (main) where

-- base
import Data.IORef
import System.Environment (lookupEnv)

-- text
import Data.Text (Text, pack)

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- sop-core
import Data.SOP (NP (..))

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Anthropic
import Control.Monad.Prompt.Tool (ToolHandler (..))
import Control.Monad.Prompt.Tool.Web (webSearchHandler)

-- test
import ConferenceTypes
import Types

main :: IO ()
main = do
  mKey <- lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> putStrLn "ANTHROPIC_API_KEY not set, skipping integration tests"
    Just key -> defaultMain $ integrationTests (pack key)

integrationTests :: Text -> TestTree
integrationTests apiKey =
  let cfg = mkAnthropicConfig apiKey
   in testGroup
        "Claude API integration"
        [ testCase "prompt returns a User" $ do
            result <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig $ do
              context "Return a JSON object for a user named Alice with email alice@example.com"
              prompt @User
            case result of
              Left err -> assertFailure (show err)
              Right user -> userName user @?= "Alice"
        , testCase "prompt returns a Counter" $ do
            result <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig $ do
              context "Return a JSON counter object with value 42."
              prompt @Counter
            case result of
              Left err -> assertFailure (show err)
              Right (Counter n) -> n @?= 42
        , testCase "conference chain produces a valid schedule" $ do
            result <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig conferenceChain
            case result of
              Left err -> assertFailure (show err)
              Right (allSpeakers, talks, schedule) -> do
                assertBool "at least 3 speakers" (length (speakers allSpeakers) >= 3)
                length talks @?= length (speakers allSpeakers)
                assertBool "at least one slot" (not (null (scheduleSlots schedule)))
        , testCase "conference chain with web_search tool: tool is invoked at least once" $ do
            callCount <- newIORef (0 :: Int)
            -- Wrap webSearchHandler to count invocations
            let countingHandler = ToolHandler $ \q -> do
                  atomicModifyIORef' callCount (\n -> (n + 1, ()))
                  runToolHandler webSearchHandler q
                handlers = countingHandler :* Nil
                -- Give the model generous retries and tool steps
                pcfg = defaultPromptConfig {maxRetries = 5, maxToolSteps = Just 10}
            result <-
              runPromptResultTWith cfg $
                runPromptT pcfg handlers conferenceChainWithTools
            case result of
              Left err -> assertFailure (show err)
              Right (allSpeakers, talks, _schedule) -> do
                assertBool "at least 3 speakers" (length (speakers allSpeakers) >= 3)
                length talks @?= length (speakers allSpeakers)
            n <- readIORef callCount
            assertBool "web_search tool was called at least once" (n > 0)
        ]
