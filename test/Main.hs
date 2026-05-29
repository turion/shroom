module Main (main) where

-- base
import Data.IORef
import Data.Proxy (Proxy (..))

-- aeson
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KM

-- text
import Data.Text (Text)

-- tasty
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt
import Control.Monad.Prompt.Ollama (schemaToFormatAndUnwrap, unwrapResult)
import Data.Describe (Describe (..), description)

-- test
import TestUtils
import Types

-- * Tests

main :: IO ()
main =
  defaultMain $
    testGroup
      "shroom"
      [ testGroup
          "deriveDescribeType"
          [ testCase "User description comes from Haddock comment" $
              describeType (Proxy @User)
                @?= "A user with a name and an email address.\n"
                  <> "- userName: The user's full name.\n"
                  <> "- userEmail: The user's email address.\n"
          , testCase "Counter description comes from Haddock comment" $
              describeType (Proxy @Counter)
                @?= "An integer counter that keeps track of how many times an event has occurred.\n"
          , testCase "Coordinate description comes from Haddock comment" $
              describeType (Proxy @Coordinate)
                @?= "A geographic coordinate expressed as a latitude/longitude pair.\n"
                  <> "- latitude: The latitude in degrees.\n"
                  <> "- longitude: The longitude in degrees.\n"
          , testCase "describeProperties works for UserEmailNotEmpty" $
              describeProperties (Proxy @User) UserEmailNotEmpty
                @?= Just "The email address is not empty."
          , testCase "propertyHolds catches empty email" $
              propertyHolds (User "Alice" "") UserEmailNotEmpty @?= False
          , testCase "propertyHolds passes non-empty email" $
              propertyHolds (User "Alice" "alice@example.com") UserEmailNotEmpty @?= True
          ]
      , testGroup
          "description output"
          [ testCase "no properties section when Property a = () (Counter)" $ do
              let d = description (Proxy @Counter)
              -- Should contain type description
              assertContains "Produce a value of the following type:" d
              -- Should NOT contain the properties header
              assertNotContains "The following invariants MUST hold in your response:" d
              -- Should contain the example
              assertContains "Example valid JSON responses:" d
          , testCase "properties section present when Property a has constructors (User)" $ do
              let d = description (Proxy @User)
              assertContains "The following invariants MUST hold in your response:" d
              assertContains "The email address is not empty." d
          ]
      , testGroup
          "schemaToFormatAndUnwrap"
          [ testCase "object schema produces identity unwrap" $ do
              let objSchema =
                    Object $
                      KM.fromList
                        [ ("type", String "object")
                        , ("properties", Object KM.empty)
                        , ("required", Array [])
                        ]
                  (_, unwrap) = schemaToFormatAndUnwrap objSchema
                  payload = "{\"name\":\"Alice\"}"
              unwrap payload @?= payload
          , testCase "integer schema wraps in result and unwraps correctly" $ do
              let intSchema = Object $ KM.fromList [("type", String "integer")]
                  (_, unwrap) = schemaToFormatAndUnwrap intSchema
                  wrapped = "{\"result\":42}"
              unwrap wrapped @?= "42"
          , testCase "string schema wraps in result and unwraps correctly" $ do
              let strSchema = Object $ KM.fromList [("type", String "string")]
                  (_, unwrap) = schemaToFormatAndUnwrap strSchema
                  wrapped = "{\"result\":\"hello\"}"
              unwrap wrapped @?= "\"hello\""
          ]
      , testGroup
          "unwrapResult"
          [ testCase "extracts result field" $
              unwrapResult "{\"result\":42}" @?= "42"
          , testCase "passes through non-object input" $
              unwrapResult "42" @?= "42"
          , testCase "passes through object without result field" $
              unwrapResult "{\"x\":1}" @?= "{\"x\":1}"
          ]
      , testGroup
          "multi-prompt context preservation"
          [ testCase "global context is visible to all prompts" $ do
              -- A mock backend that records contexts it receives
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts

              _ <- runPromptResultTWith mockCfg $ runPromptT defaultPromptConfig $ do
                context "global info"
                _ <- prompt @Counter
                _ <- prompt @Counter
                pure ()

              seen <- readIORef seenContexts
              -- Both calls should have seen the global context
              length seen @?= 2
              all (\c -> "global info" `textIsInfixOf` c) seen @?= True
          , testCase "promptWith local context not carried forward" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts

              _ <- runPromptResultTWith mockCfg $ runPromptT defaultPromptConfig $ do
                context "global"
                _ <- promptWith @Counter "local only"
                _ <- prompt @Counter
                pure ()

              seen <- readIORef seenContexts
              case seen of
                [firstCall, secondCall] -> do
                  -- First call: has both global and local
                  "local only" `textIsInfixOf` firstCall @?= True
                  -- Second call: only global, no local
                  "local only" `textIsInfixOf` secondCall @?= False
                _ -> assertFailure "Unexpected number of seen contexts"
          ]
      , testGroup
          "parallel prompts"
          [ testCase "promptPar makes two LLM calls" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              result <-
                runPromptResultTWith mockCfg $
                  runPromptT defaultPromptConfig $
                    promptPar (prompt @Counter) (prompt @Counter)
              case result of
                Left err -> fail (show err)
                Right _ -> pure ()
              seen <- readIORef seenContexts
              length seen @?= 2
          , testCase "promptPar branches both see global context" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <- runPromptResultTWith mockCfg $ runPromptT defaultPromptConfig $ do
                context "shared global"
                promptPar (prompt @Counter) (prompt @Counter)
              seen <- readIORef seenContexts
              all ("shared global" `textIsInfixOf`) seen @?= True
          , testCase "context inside promptPar branch does not leak to sibling" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <-
                runPromptResultTWith mockCfg $
                  runPromptT defaultPromptConfig $
                    promptPar
                      (context "branch-local" >> prompt @Counter)
                      (prompt @Counter :: PromptT IO Counter)
              seen <- readIORef seenContexts
              -- exactly one branch should have seen "branch-local"
              length (filter ("branch-local" `textIsInfixOf`) seen) @?= 1
          , testCase "context inside promptPar branch does not leak to subsequent sequential prompt" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <- runPromptResultTWith mockCfg $ runPromptT defaultPromptConfig $ do
                _ <-
                  promptPar
                    (context "branch-local" >> prompt @Counter)
                    (prompt @Counter)
                prompt @Counter
              seen <- readIORef seenContexts
              -- last (3rd) call must not contain "branch-local"
              "branch-local" `textIsInfixOf` (seen !! 2) @?= False
          , testCase "promptsParallel makes one call per element" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              result <-
                runPromptResultTWith mockCfg $
                  runPromptT defaultPromptConfig $
                    promptsParallel (replicate 3 (prompt @Counter))
              case result of
                Left err -> fail (show err)
                Right xs -> length xs @?= 3
              seen <- readIORef seenContexts
              length seen @?= 3
          ]
      , testGroup
          "property validation and retry"
          [ testCase "validation passes immediately — no retry" $ do
              -- Valid user (non-empty email): should succeed in one call
              responses <- newIORef ["{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"]
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptT (PromptConfig {maxRetries = 2}) $ prompt @User
              result @?= Right (User "Alice" "alice@example.com")
              seen <- readIORef seenContexts
              length seen @?= 1
          , testCase "validation fails then passes — retry succeeds" $ do
              -- First response: invalid (empty email); second: valid
              responses <-
                newIORef
                  [ "{\"userName\":\"Alice\",\"userEmail\":\"\"}"
                  , "{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"
                  ]
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptT (PromptConfig {maxRetries = 2}) $ prompt @User
              result @?= Right (User "Alice" "alice@example.com")
              seen <- readIORef seenContexts
              length seen @?= 2
          , testCase "retry context includes failure description" $ do
              responses <-
                newIORef
                  [ "{\"userName\":\"Alice\",\"userEmail\":\"\"}"
                  , "{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"
                  ]
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              _ <- runPromptResultTWith cfg $ runPromptT (PromptConfig {maxRetries = 2}) $ prompt @User
              seen <- readIORef seenContexts
              -- Second context should include the failure description
              assertContains "The email address is not empty." (seen !! 1)
              assertContains "Generate a new, corrected JSON response" (seen !! 1)
          , testCase "validation always fails — error after retries exhausted" $ do
              -- Always returns invalid user
              responses <- newIORef (repeat "{\"userName\":\"Alice\",\"userEmail\":\"\"}")
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptT (PromptConfig {maxRetries = 2}) $ prompt @User
              case result of
                Left err -> assertContains "The email address is not empty." err
                Right _ -> fail "Expected Left but got Right"
              seen <- readIORef seenContexts
              -- 1 original + 2 retries = 3 total calls
              length seen @?= 3
          , testCase "JSON decode error triggers retry" $ do
              responses <-
                newIORef
                  [ "not valid json"
                  , "{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"
                  ]
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptT (PromptConfig {maxRetries = 2}) $ prompt @User
              result @?= Right (User "Alice" "alice@example.com")
              seen <- readIORef seenContexts
              length seen @?= 2
              assertContains "IMPORTANT" (seen !! 1)
              assertContains "not valid json" (seen !! 1)
          , testCase "JSON decode error exhausts retries" $ do
              responses <- newIORef (repeat "not valid json")
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptT (PromptConfig {maxRetries = 1}) $ prompt @User
              case result of
                Left err -> assertContains "JSON decode error" err
                Right _ -> fail "Expected Left but got Right"
              seen <- readIORef seenContexts
              -- 1 original + 1 retry = 2 total calls
              length seen @?= 2
          ]
      ]
