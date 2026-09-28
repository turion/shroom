module Main (main) where

-- base
import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), forM_)
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
import Control.Monad.Prompt.Tool.Web
import Data.Shroom.Class (Describable (..), Surveyable (..), description)

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
              assertNotContains "The following properties MUST hold in your response:" d
              -- Should contain the example
              assertContains "Example valid JSON responses:" d
          , testCase "properties section present when Property a has constructors (User)" $ do
              let d = description (Proxy @User)
              assertContains "The following properties MUST hold in your response:" d
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

              _ <- runPromptResultTWith mockCfg $ runPromptTNoTools defaultPromptConfig $ do
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

              _ <- runPromptResultTWith mockCfg $ runPromptTNoTools defaultPromptConfig $ do
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
                  runPromptTNoTools defaultPromptConfig $
                    promptPar (prompt @Counter) (prompt @Counter)
              case result of
                Left err -> fail (show err)
                Right _ -> pure ()
              seen <- readIORef seenContexts
              length seen @?= 2
          , testCase "promptPar branches both see global context" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <- runPromptResultTWith mockCfg $ runPromptTNoTools defaultPromptConfig $ do
                context "shared global"
                promptPar (prompt @Counter) (prompt @Counter)
              seen <- readIORef seenContexts
              all ("shared global" `textIsInfixOf`) seen @?= True
          , testCase "context inside promptPar branch does not leak to sibling" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <-
                runPromptResultTWith mockCfg $
                  runPromptTNoTools defaultPromptConfig $
                    promptPar
                      (context "branch-local" >> prompt @Counter)
                      (prompt @Counter :: PromptT IO Counter)
              seen <- readIORef seenContexts
              -- exactly one branch should have seen "branch-local"
              length (filter ("branch-local" `textIsInfixOf`) seen) @?= 1
          , testCase "context inside promptPar branch does not leak to subsequent sequential prompt" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <- runPromptResultTWith mockCfg $ runPromptTNoTools defaultPromptConfig $ do
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
                  runPromptTNoTools defaultPromptConfig $
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
              result <- runPromptResultTWith cfg $ runPromptTNoTools (defaultPromptConfig {maxRetries = 2}) $ prompt @User
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
              result <- runPromptResultTWith cfg $ runPromptTNoTools (defaultPromptConfig {maxRetries = 2}) $ prompt @User
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
              _ <- runPromptResultTWith cfg $ runPromptTNoTools (defaultPromptConfig {maxRetries = 2}) $ prompt @User
              seen <- readIORef seenContexts
              -- Second context should include the failure description
              assertContains "The email address is not empty." (seen !! 1)
              assertContains "Generate a new, corrected JSON response" (seen !! 1)
          , testCase "validation always fails — error after retries exhausted" $ do
              -- Always returns invalid user
              responses <- newIORef (repeat "{\"userName\":\"Alice\",\"userEmail\":\"\"}")
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptTNoTools (defaultPromptConfig {maxRetries = 2}) $ prompt @User
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
              result <- runPromptResultTWith cfg $ runPromptTNoTools (defaultPromptConfig {maxRetries = 2}) $ prompt @User
              result @?= Right (User "Alice" "alice@example.com")
              seen <- readIORef seenContexts
              length seen @?= 2
              assertContains "IMPORTANT" (seen !! 1)
              assertContains "not valid json" (seen !! 1)
          , testCase "JSON decode error exhausts retries" $ do
              responses <- newIORef (repeat "not valid json")
              seenContexts <- newIORef ([] :: [Text])
              let cfg = SeqMockConfig responses seenContexts
              result <- runPromptResultTWith cfg $ runPromptTNoTools (defaultPromptConfig {maxRetries = 1}) $ prompt @User
              case result of
                Left err -> assertContains "JSON decode error" err
                Right _ -> fail "Expected Left but got Right"
              seen <- readIORef seenContexts
              -- 1 original + 1 retry = 2 total calls
              length seen @?= 2
          ]
      , testGroup
          "Alternative, MonadPlus, MonadFail"
          [ testCase "empty always fails" $ do
              let cfg = MockConfig (error "not implemented")
              result <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig (empty :: PromptT IO Counter)
              case result of
                Left _ -> pure ()
                Right _ -> fail "Expected Left"
          , testCase "Fail propagates the message" $ do
              let cfg = MockConfig (error "not implemented")
              result <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig (Fail "the reason" :: PromptT IO Counter)
              case result of
                Left err -> assertContains "the reason" err
                Right _ -> fail "Expected Left"
          , testCase "MonadFail propagates the message" $ do
              let cfg = MockConfig (error "not implemented")
              result <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig (fail "oops" :: PromptT IO Counter)
              case result of
                Left err -> assertContains "oops" err
                Right _ -> fail "Expected Left"
          , testCase "succeeding first branch returned, fallback not run" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                runPromptResultTWith cfg $
                  runPromptTNoTools
                    defaultPromptConfig
                    (Pure (Counter 42) <|> Fail "should not reach")
              case result of
                Right (Counter 42) -> pure ()
                _ -> fail "Expected Right (Counter 42)"
          , testCase "failing first branch falls back to second" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                runPromptResultTWith cfg $
                  runPromptTNoTools
                    defaultPromptConfig
                    (Fail "x" <|> Pure (Counter 42))
              case result of
                Right (Counter 42) -> pure ()
                _ -> fail "Expected Right (Counter 42)"
          , testCase "both branches fail — second error propagates" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                runPromptResultTWith cfg $
                  runPromptTNoTools
                    defaultPromptConfig
                    (Fail "first" <|> (Fail "second" :: PromptT IO Counter))
              case result of
                Left err -> assertContains "second" err
                Right _ -> fail "Expected Left"
          , testCase "backend error triggers fallback" $ do
              result <-
                runPromptResultTWith InjectFailConfig $
                  runPromptTNoTools
                    defaultPromptConfig
                    (prompt @Counter <|> Pure (Counter 0))
              case result of
                Right (Counter 0) -> pure ()
                _ -> fail "Expected Right (Counter 0)"
          , testCase "context from failing branch does not persist" $ do
              seenContexts <- newIORef ([] :: [Text])
              let cfg = MockConfig seenContexts
              _ <-
                runPromptResultTWith cfg $
                  runPromptTNoTools defaultPromptConfig $
                    (context "leaked" >> Fail "x") <|> prompt @Counter
              seen <- readIORef seenContexts
              length seen @?= 1
              forM_ seen $ assertNotContains "leaked"
          , testCase "context from winning branch persists to next prompt" $ do
              seenContexts <- newIORef ([] :: [Text])
              let cfg = MockConfig seenContexts
              _ <- runPromptResultTWith cfg $ runPromptTNoTools defaultPromptConfig $ do
                _ <- (context "kept" >> prompt @Counter) <|> Fail "x"
                prompt @Counter
              seen <- readIORef seenContexts
              length seen @?= 2
              assertContains "kept" (seen !! 1)
          , testCase "mzero `mplus` p equals p" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                runPromptResultTWith cfg $
                  runPromptTNoTools
                    defaultPromptConfig
                    (mzero `mplus` Pure (Counter 99))
              case result of
                Right (Counter 99) -> pure ()
                _ -> fail "Expected Right (Counter 99)"
          ]
      , testGroup
          "web tool security properties"
          [ -- WebFetch URL scheme
            testCase "valid https URL passes scheme check" $
              propertyHolds (WebFetch "https://example.com") WebFetchUrlScheme @?= True
          , testCase "valid http URL passes scheme check" $
              propertyHolds (WebFetch "http://example.com") WebFetchUrlScheme @?= True
          , testCase "file:// URL fails scheme check" $
              propertyHolds (WebFetch "file:///etc/passwd") WebFetchUrlScheme @?= False
          , testCase "empty string fails scheme check" $
              propertyHolds (WebFetch "") WebFetchUrlScheme @?= False
          , -- WebFetch safe chars
            testCase "clean URL passes safe-char check" $
              propertyHolds (WebFetch "https://example.com/path?q=1&x=2") WebFetchUrlSafeChars @?= True
          , testCase "URL with space fails safe-char check" $
              propertyHolds (WebFetch "https://example.com/bad url") WebFetchUrlSafeChars @?= False
          , testCase "URL with backtick injection fails safe-char check" $
              propertyHolds (WebFetch "https://example.com/`rm -rf /`") WebFetchUrlSafeChars @?= False
          , testCase "URL with angle bracket fails safe-char check" $
              propertyHolds (WebFetch "https://evil.com/<script>") WebFetchUrlSafeChars @?= False
          , -- DuckDuckGoSearch not-empty
            testCase "non-empty DDG query passes not-empty check" $
              propertyHolds (DuckDuckGoSearch "Haskell") DuckDuckGoSearchQueryNotEmpty @?= True
          , testCase "empty DDG query fails not-empty check" $
              propertyHolds (DuckDuckGoSearch "") DuckDuckGoSearchQueryNotEmpty @?= False
          , -- DuckDuckGoSearch safe chars
            testCase "plain DDG query passes safe-char check" $
              propertyHolds (DuckDuckGoSearch "Simon Peyton Jones") DuckDuckGoSearchQuerySafeChars @?= True
          , testCase "DDG query with newline fails safe-char check" $
              propertyHolds (DuckDuckGoSearch "foo\nbar") DuckDuckGoSearchQuerySafeChars @?= False
          , testCase "DDG query with angle bracket fails safe-char check" $
              propertyHolds (DuckDuckGoSearch "foo <script>") DuckDuckGoSearchQuerySafeChars @?= False
          , testCase "DDG query with semicolon fails safe-char check" $
              propertyHolds (DuckDuckGoSearch "foo; rm -rf /") DuckDuckGoSearchQuerySafeChars @?= False
          , -- WikipediaSearch safe chars
            testCase "plain Wikipedia query passes safe-char check" $
              propertyHolds (WikipediaSearch "functional programming") WikipediaSearchQuerySafeChars @?= True
          , testCase "Wikipedia query with newline fails safe-char check" $
              propertyHolds (WikipediaSearch "foo\nbar") WikipediaSearchQuerySafeChars @?= False
          , testCase "empty Wikipedia query fails not-empty check" $
              propertyHolds (WikipediaSearch "") WikipediaSearchQueryNotEmpty @?= False
          ]
      ]
