{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

-- base
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import System.Exit (die)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- openapi3
import Data.OpenApi (ToSchema)

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

-- shroom
import Control.Monad.Prompt (Promptable)
import Control.Monad.Prompt.Baikai (claudeBackend)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.Tool (ToolHandler (..), Toolable (..), runTool)
import Control.Monad.Prompt.Tool.Web (
  DuckDuckGoSearch,
  WebFetch,
  WikipediaSearch,
  duckDuckGoSearchHandler,
  webFetchHandler,
  wikipediaSearchHandler,
 )

import Data.Shroom.Class (Describable (..), Surveyable (..))

-- test
import Integration.Cases (backendIntegrationTests)
import WebToolReport (toolResults, webToolReportChain)

-- * Fake broken tool for testing failure reporting

newtype FakeBrokenSearch = FakeBrokenSearch {brokenQuery :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Describable FakeBrokenSearch where
  describeType _ = "A search query that always fails with a 404 error."

instance Surveyable FakeBrokenSearch

instance Promptable FakeBrokenSearch

instance Toolable FakeBrokenSearch where
  toolDescription _ = Just "Always returns an HTTP 404 error."

fakeBrokenSearchHandler :: ToolHandler FakeBrokenSearch
fakeBrokenSearchHandler = ToolHandler $ \_ -> pure (Left "HTTP 404: not found")

main :: IO ()
main = do
  mKey <- lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> die "ANTHROPIC_API_KEY not set; the Claude integration suite needs it to run"
    Just key -> integrationTests (pack key) >>= defaultMain

integrationTests :: Text -> IO TestTree
integrationTests apiKey = do
  backend <- claudeBackend apiKey
  pure $
    testGroup
      "Claude API integration"
      [ backendIntegrationTests backend
      , testCase "web tools smoke test: all three tools work" $ do
          let pcfg = defaultPromptConfig {maxRetries = 3, maxToolSteps = Just 15, debugLog = Just (putStrLn . T.unpack)}
          result <-
            Effect.runPromptResultEff backend pcfg $
              runTool duckDuckGoSearchHandler $
                runTool wikipediaSearchHandler $
                  runTool webFetchHandler $
                    webToolReportChain @'[DuckDuckGoSearch, WikipediaSearch, WebFetch]
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
          let pcfg = defaultPromptConfig {maxRetries = 3, maxToolSteps = Just 5, debugLog = Just (putStrLn . T.unpack)}
          result <-
            Effect.runPromptResultEff backend pcfg $
              runTool fakeBrokenSearchHandler (webToolReportChain @'[FakeBrokenSearch])
          case result of
            Left err -> assertFailure (T.unpack err)
            Right report ->
              case lookup "fake_broken_search" (toolResults report) of
                Nothing -> assertFailure "fake_broken_search missing from report"
                Just "ok" -> assertFailure "Expected fake_broken_search to be reported as failed, but got ok"
                Just _ -> pure () -- model correctly reported a non-ok status
      ]
