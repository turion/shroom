{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

-- base
import Data.IORef
import GHC.Generics (Generic)
import System.Environment (lookupEnv)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- sop-core
import Data.SOP (NP (..))

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Anthropic
import Control.Monad.Prompt.Tool (ToolHandler (..), Toolable (..))
import Control.Monad.Prompt.Tool.Web (
  DuckDuckGoSearch,
  WebFetch,
  WikipediaSearch,
  duckDuckGoSearchHandler,
  webFetchHandler,
  wikipediaSearchHandler,
 )
import Data.Shroom.Class (Describable (..), DescriptionPrompt, Promptable, Surveyable (..))

-- test
import ConferenceTypes
import Types
import WebToolReport (toolResults, webToolReportChain)

-- * Fake broken tool for testing failure reporting

newtype FakeBrokenSearch = FakeBrokenSearch {brokenQuery :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Describable FakeBrokenSearch where
  describeType _ = "A search query that always fails with a 404 error."

instance Surveyable FakeBrokenSearch

deriving via DescriptionPrompt FakeBrokenSearch instance Promptable FakeBrokenSearch

instance Toolable FakeBrokenSearch where
  toolDescription _ = Just "Always returns an HTTP 404 error."

fakeBrokenSearchHandler :: ToolHandler FakeBrokenSearch
fakeBrokenSearchHandler = ToolHandler $ \_ -> pure (Left "HTTP 404: not found")

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
        , testCase "web tools smoke test: all three tools work" $ do
            let handlers = duckDuckGoSearchHandler :* wikipediaSearchHandler :* webFetchHandler :* Nil
                pcfg = defaultPromptConfig {maxRetries = 3, maxToolSteps = Just 15, debugLog = Just (putStrLn . T.unpack)}
            result <-
              runPromptResultTWith cfg $
                runPromptT pcfg handlers (webToolReportChain @'[DuckDuckGoSearch, WikipediaSearch, WebFetch])
            case result of
              Left err -> assertFailure (T.unpack err)
              Right report ->
                -- "ok" means success; anything else is a reported failure
                mapM_
                  ( \(tool, status) ->
                      if status == "ok"
                        then pure ()
                        else assertFailure ("Tool " <> T.unpack tool <> " failed: " <> T.unpack status)
                  )
                  (toolResults report)
        , testCase "web tools: broken tool failure is reported" $ do
            let handlers = fakeBrokenSearchHandler :* Nil
                pcfg = defaultPromptConfig {maxRetries = 3, maxToolSteps = Just 5, debugLog = Just (putStrLn . T.unpack)}
            result <-
              runPromptResultTWith cfg $
                runPromptT pcfg handlers (webToolReportChain @'[FakeBrokenSearch])
            case result of
              Left err -> assertFailure (T.unpack err)
              Right report ->
                case lookup "fake_broken_search" (toolResults report) of
                  Nothing -> assertFailure "fake_broken_search missing from report"
                  Just "ok" -> assertFailure "Expected fake_broken_search to be reported as failed, but got ok"
                  Just _ -> pure () -- model correctly reported a non-ok status
        , testCase "conference chain with web_search tool: tool is invoked at least once" $ do
            callCount <- newIORef (0 :: Int)
            -- Wrap webSearchHandler to count invocations
            let countingHandler = ToolHandler $ \q -> do
                  atomicModifyIORef' callCount (\n -> (n + 1, ()))
                  runToolHandler duckDuckGoSearchHandler q
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
