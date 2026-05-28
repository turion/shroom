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

-- test
import Types

main :: IO ()
main = do
  mKey <- lookupEnv "ANTHROPIC_API_KEY"
  case mKey of
    Nothing -> putStrLn "ANTHROPIC_API_KEY not set, skipping integration tests"
    Just key -> defaultMain $ integrationTests (pack key)

integrationTests :: Text -> TestTree
integrationTests apiKey =
  testGroup
    "PromptT integration"
    [ testCase "prompt returns a User" $ do
        let cfg = PromptConfig {apiKey = apiKey, model = "claude-3-5-haiku-20241022"}
        result <- runPromptResultTWith cfg $ runPromptT $ do
          context "Return a JSON object for a user named Alice with email alice@example.com"
          prompt @User
        case result of
          Left err -> assertFailure (show err)
          Right user -> userName user @?= "Alice"
    , testCase "prompt returns a Counter" $ do
        let cfg = PromptConfig {apiKey = apiKey, model = "claude-3-5-haiku-20241022"}
        result <- runPromptResultTWith cfg $ runPromptT $ do
          context "Return a JSON counter object with value 42."
          prompt @Counter
        case result of
          Left err -> assertFailure (show err)
          Right (Counter n) -> n @?= 42
    ]
