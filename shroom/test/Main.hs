{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

-- base
import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (bracket)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Proxy (Proxy (..))
import GHC.Clock (getMonotonicTime)
import System.Timeout (timeout)

-- aeson
import Data.Aeson (FromJSON, ToJSON, Value (..), toJSON)
import Data.Aeson.Text (encodeToLazyText)

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL

-- GHC.Generics
import GHC.Generics (Generic)

-- network
import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (SockAddrInet),
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultProtocol,
  getSocketName,
  listen,
  socket,
  tupleToHostAddress,
 )

-- openapi3
import Data.OpenApi (ToSchema)

-- sop-core
import Data.SOP (NP (..))

-- tasty
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- effectful
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State)

-- shroom
import Control.Monad.Prompt.Backend (Backend (..), BackendReply (..), ContextItem (..), ToolCall (..), ToolDef (..))
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Eff
import Control.Monad.Prompt.Promptable (Promptable)
import Control.Monad.Prompt.Tool (Tool, ToolHandler (..), Toolable, runTool, toolBinding, toolDefsRaw, toolName)
import Control.Monad.Prompt.Tool.Web
import Data.Shroom.Class (Describable (..), Surveyable (..), description)

-- test
import TestUtils
import Types

-- * Mock backends

{- | A mock 'Backend' that blocks every call until @expected@ calls have
all entered, then lets them all through together.

This proves genuine concurrency without a timing assertion: if the calling
branches ran one after another rather than concurrently, the first call
would still be blocked waiting for a second call that has not even started
yet, so the whole computation hangs until the test's outer 'timeout' kills
it. There is no race window to get unlucky on — under real concurrency the
wait resolves as soon as the last branch starts; under sequential
execution it never resolves at all.
-}
barrierBackend :: (MonadIO m) => MVar Int -> Int -> Backend m
barrierBackend entered expected =
  Backend $ \_ctx _typeDesc _schema _toolDefs -> liftIO $ do
    modifyMVar_ entered (pure . (+ 1))
    let waitForAll = do
          n <- readMVar entered
          if n >= expected then pure () else threadDelay 1000 >> waitForAll
    waitForAll
    pure (Right (BackendAnswer "0"))

{- | A mock 'Backend' that requests the given tool call once, then answers
@"0"@ (a valid 'Counter') once the conversation shows a tool result. Proves
a tool result actually reaches its registered handler through
"Control.Monad.Prompt.Effect"\'s own tool loop, without needing a real
backend's wire-level tool_use protocol.
-}
toolCallBackend :: (MonadIO m) => Text -> Value -> Backend m
toolCallBackend name inputVal =
  Backend $ \ctx _typeDesc _schema _toolDefs ->
    pure $
      Right $
        if any isToolResult ctx
          then BackendAnswer "0"
          else BackendToolCalls [ToolCall {toolCallId = "call-id", toolCallName = name, toolCallArguments = inputVal}]
  where
    isToolResult (ToolResultMessage _) = True
    isToolResult _ = False

{- | A mock 'Backend' that requests the given tool for the first @n@ rounds,
then answers directly — exercising "Control.Monad.Prompt.Effect"\'s own
tool loop's budget rules (a round costs a step only on success, failed
calls are free) rather than re-testing them against a loop in isolation.

Honours the same "empty @toolDefs@ means no tools are being offered"
contract 'Control.Monad.Prompt.Backend.Backend' documents: called with no
tools (the exhaustion path's own tool-free call), it always answers
straight away rather than consulting the round counter, exactly as the
pre-'Backend'-adapter mock's separate @callModelNoTools@ field always did.
Without this, the round counter alone would keep offering tool calls past
the caller's own step budget, and the tool-free call would look like a
backend that ignored being asked not to.
-}
roundBasedToolBackend :: (MonadIO m) => Text -> Int -> IORef Int -> Backend m
roundBasedToolBackend name wantRounds roundRef =
  Backend $ \_ctx _typeDesc _schema toolDefs ->
    if null toolDefs
      then pure (Right (BackendAnswer "0"))
      else liftIO $ do
        n <- readIORef roundRef
        modifyIORef' roundRef (+ 1)
        pure $
          Right $
            if n < wantRounds
              then BackendToolCalls [ToolCall {toolCallId = "call-id", toolCallName = name, toolCallArguments = toJSON (DuckDuckGoSearch "budget-test")}]
              else BackendAnswer "0"

{- | The effect stack every mock test below runs its program through, once
its own tools have been peeled off the front by 'runTool' — exactly what
'Eff.runPromptResultEff' expects.
-}
type BaseEs = '[Eff.Prompt, State [ContextItem], Error Text, Concurrent, IOE]

{- | Demonstrates "a sub-program can be run with a strict subset of its
caller's tools": this needs only 'DuckDuckGoSearch', not 'WikipediaSearch'.
-}
subProgram :: (Tool DuckDuckGoSearch :> es, Eff.Prompt :> es) => Eff es Counter
subProgram = Eff.promptTools [toolBinding @DuckDuckGoSearch]

{- | A caller with a wider tool surface than 'subProgram' needs. Delegating
to it type-checks for free, because its @es@ is a strict superset of
'subProgram'\'s.
-}
callerProgram :: Eff (Tool DuckDuckGoSearch : Tool WikipediaSearch : BaseEs) Counter
callerProgram = subProgram

-- * A nested array-of-records type, for 'schemaWithDefs' regression coverage

{- | A record two named-schema hops away from a would-be top level — the
shape 'Quexnorbs' below needs, and the shape the existing suite never had:
a nested array of records referencing another record. Field names are
deliberately made up (matching todo 20's own diagnostic vocabulary) so a
passing test can only mean the schema carried them, never that a model
guessed something plausible.
-}
data Zblorf = Zblorf
  { zblorfSnorkfirst :: Text
  , zblorfSnorklast :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | A record containing 'Zblorf' — one named-schema hop further in.
data Quexnorb = Quexnorb
  { quexnorbVintquark :: Zblorf
  , quexnorbLabel :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

{- | The actual type under test: an array of 'Quexnorb', itself referencing
'Zblorf'. This is what 'ConferenceTypes.Speakers' looks like structurally
(a list of records, each holding another record) without depending on
that module, so this test stays a fast, hermetic unit test rather than a
live-model integration one.
-}
newtype Quexnorbs = Quexnorbs {quexnorbs :: [Quexnorb]}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Describable Zblorf where describeType _ = "A record with two unguessable-name fields."
instance Surveyable Zblorf
instance Promptable Zblorf

instance Describable Quexnorb where describeType _ = "A record containing another record."
instance Surveyable Quexnorb
instance Promptable Quexnorb

instance Describable Quexnorbs where describeType _ = "A list of records, each containing another record."
instance Surveyable Quexnorbs
instance Promptable Quexnorbs

instance Toolable Quexnorbs

{- | Run an action against a localhost TCP listener that accepts a connection
and then never reads from it or writes to it. Binds port 0, so the OS picks a
free one and parallel runs cannot collide; localhost only, no external
service.
-}
withSilentServer :: (PortNumber -> IO a) -> IO a
withSilentServer act =
  bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen listener 1
    port <-
      getSocketName listener >>= \case
        SockAddrInet p _ -> pure p
        other -> fail ("unexpected listener address: " <> show other)
    -- Hold the accepted connection open, silent, until the thread is killed.
    blocked <- newEmptyMVar @()
    bracket
      ( forkIO . void $ do
          (conn, _) <- accept listener
          bracket (pure conn) close (\_ -> takeMVar blocked)
      )
      killThread
      (\_ -> act port)

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
          "structured-output schema (Control.Monad.Prompt.Schema.schemaWithDefs, via toolDefsRaw)"
          [ testCase "a nested array-of-records type carries no unresolved $ref/$defs, and keeps its field names" $ do
              let dummyHandler = ToolHandler $ \_ -> pure (Right "")
              -- The bug this guards against (todo 20): a live Ollama resolves one
              -- '$ref' hop correctly but silently stops enforcing the schema at a
              -- second one — exactly what a nested array-of-records type produces
              -- unless the schema handed to the wire is fully self-contained.
              case toolDefsRaw (dummyHandler :* Nil :: NP ToolHandler '[Quexnorbs]) of
                [toolDef] -> do
                  let schemaText = TL.toStrict (encodeToLazyText (toolDefSchema toolDef))
                  assertNotContains "$ref" schemaText
                  assertNotContains "$defs" schemaText
                  assertContains "quexnorbVintquark" schemaText
                  assertContains "zblorfSnorkfirst" schemaText
                  assertContains "zblorfSnorklast" schemaText
                defs -> assertFailure ("expected exactly one ToolDef, got " <> show (length defs))
          ]
      , testGroup
          "multi-prompt context preservation"
          [ testCase "global context is visible to all prompts" $ do
              -- A mock backend that records contexts it receives
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts

              _ <- Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $ do
                Eff.context "global info"
                _ <- Eff.prompt @Counter
                _ <- Eff.prompt @Counter
                pure ()

              seen <- readIORef seenContexts
              -- Both calls should have seen the global context
              length seen @?= 2
              all (\c -> "global info" `textIsInfixOf` c) seen @?= True
          , testCase "promptWith local context not carried forward" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts

              _ <- Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $ do
                Eff.context "global"
                _ <- Eff.promptWith @Counter "local only"
                _ <- Eff.prompt @Counter
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
                Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $
                  Eff.promptPar (Eff.prompt @Counter) (Eff.prompt @Counter)
              case result of
                Left err -> fail (show err)
                Right _ -> pure ()
              seen <- readIORef seenContexts
              length seen @?= 2
          , testCase "promptPar branches both see global context" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <- Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $ do
                Eff.context "shared global"
                Eff.promptPar (Eff.prompt @Counter) (Eff.prompt @Counter)
              seen <- readIORef seenContexts
              all ("shared global" `textIsInfixOf`) seen @?= True
          , testCase "context inside promptPar branch does not leak to sibling" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <-
                Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $
                  Eff.promptPar
                    (Eff.context "branch-local" >> Eff.prompt @Counter)
                    (Eff.prompt @Counter)
              seen <- readIORef seenContexts
              -- exactly one branch should have seen "branch-local"
              length (filter ("branch-local" `textIsInfixOf`) seen) @?= 1
          , testCase "context inside promptPar branch does not leak to subsequent sequential prompt" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              _ <- Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $ do
                _ <-
                  Eff.promptPar
                    (Eff.context "branch-local" >> Eff.prompt @Counter)
                    (Eff.prompt @Counter)
                Eff.prompt @Counter
              seen <- readIORef seenContexts
              -- last (3rd) call must not contain "branch-local"
              "branch-local" `textIsInfixOf` (seen !! 2) @?= False
          , testCase "promptsParallel makes one call per element" $ do
              seenContexts <- newIORef ([] :: [Text])
              let mockCfg = MockConfig seenContexts
              result <-
                Eff.runPromptResultEff (mockBackend mockCfg) defaultPromptConfig $
                  Eff.promptsParallel (replicate 3 (Eff.prompt @Counter))
              case result of
                Left err -> fail (show err)
                Right xs -> length xs @?= 3
              seen <- readIORef seenContexts
              length seen @?= 3
          ]
      , testGroup
          "failure and fallback"
          [ testCase "failWith propagates the message" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                Eff.runPromptResultEff (mockBackend cfg) defaultPromptConfig (Eff.failWith "the reason" :: Eff BaseEs Counter)
              case result of
                Left err -> assertContains "the reason" err
                Right _ -> fail "Expected Left"
          , testCase "succeeding first branch returned, fallback not run" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                Eff.runPromptResultEff (mockBackend cfg) defaultPromptConfig $
                  pure (Counter 42) `Eff.orElse` Eff.failWith "should not reach"
              case result of
                Right (Counter 42) -> pure ()
                _ -> fail "Expected Right (Counter 42)"
          , testCase "failing first branch falls back to second" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                Eff.runPromptResultEff (mockBackend cfg) defaultPromptConfig $
                  Eff.failWith "x" `Eff.orElse` pure (Counter 42)
              case result of
                Right (Counter 42) -> pure ()
                _ -> fail "Expected Right (Counter 42)"
          , testCase "both branches fail — second error propagates" $ do
              let cfg = MockConfig (error "not implemented")
              result <-
                Eff.runPromptResultEff (mockBackend cfg) defaultPromptConfig $
                  Eff.failWith "first" `Eff.orElse` (Eff.failWith "second" :: Eff BaseEs Counter)
              case result of
                Left err -> assertContains "second" err
                Right _ -> fail "Expected Left"
          , testCase "backend error triggers fallback" $ do
              result <-
                Eff.runPromptResultEff (injectFailBackend InjectFailConfig) defaultPromptConfig $
                  Eff.prompt @Counter `Eff.orElse` pure (Counter 0)
              case result of
                Right (Counter 0) -> pure ()
                _ -> fail "Expected Right (Counter 0)"
          , testCase "context from failing branch does not persist" $ do
              seenContexts <- newIORef ([] :: [Text])
              let cfg = MockConfig seenContexts
              _ <-
                Eff.runPromptResultEff (mockBackend cfg) defaultPromptConfig $
                  (Eff.context "leaked" >> Eff.failWith "x") `Eff.orElse` Eff.prompt @Counter
              seen <- readIORef seenContexts
              length seen @?= 1
              forM_ seen $ assertNotContains "leaked"
          , testCase "context from winning branch persists to next prompt" $ do
              seenContexts <- newIORef ([] :: [Text])
              let cfg = MockConfig seenContexts
              _ <- Eff.runPromptResultEff (mockBackend cfg) defaultPromptConfig $ do
                _ <- (Eff.context "kept" >> Eff.prompt @Counter) `Eff.orElse` Eff.failWith "x"
                Eff.prompt @Counter
              seen <- readIORef seenContexts
              length seen @?= 2
              assertContains "kept" (seen !! 1)
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
      , testGroup
          "Prompt effect (effectful)"
          [ testGroup
              "retry loop"
              [ testCase "validation passes immediately — no retry" $ do
                  responses <- newIORef ["{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"]
                  seenContexts <- newIORef ([] :: [Text])
                  let cfg = SeqMockConfig responses seenContexts
                  result <-
                    Eff.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) $
                      Eff.prompt @User
                  result @?= Right (User "Alice" "alice@example.com")
                  seen <- readIORef seenContexts
                  length seen @?= 1
              , testCase "validation fails then passes — retry succeeds" $ do
                  responses <-
                    newIORef
                      [ "{\"userName\":\"Alice\",\"userEmail\":\"\"}"
                      , "{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"
                      ]
                  seenContexts <- newIORef ([] :: [Text])
                  let cfg = SeqMockConfig responses seenContexts
                  result <-
                    Eff.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) $
                      Eff.prompt @User
                  result @?= Right (User "Alice" "alice@example.com")
                  seen <- readIORef seenContexts
                  length seen @?= 2
              , testCase "retry context includes the property-violation description" $ do
                  responses <-
                    newIORef
                      [ "{\"userName\":\"Alice\",\"userEmail\":\"\"}"
                      , "{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"
                      ]
                  seenContexts <- newIORef ([] :: [Text])
                  let cfg = SeqMockConfig responses seenContexts
                  _ <-
                    Eff.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) $
                      Eff.prompt @User
                  seen <- readIORef seenContexts
                  assertContains "The email address is not empty." (seen !! 1)
                  assertContains "Generate a new, corrected JSON response" (seen !! 1)
              , testCase "JSON decode error triggers retry — distinct wording from a property violation" $ do
                  responses <-
                    newIORef
                      [ "not valid json"
                      , "{\"userName\":\"Alice\",\"userEmail\":\"alice@example.com\"}"
                      ]
                  seenContexts <- newIORef ([] :: [Text])
                  let cfg = SeqMockConfig responses seenContexts
                  result <-
                    Eff.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) $
                      Eff.prompt @User
                  result @?= Right (User "Alice" "alice@example.com")
                  seen <- readIORef seenContexts
                  length seen @?= 2
                  assertContains "IMPORTANT: Your previous response could not be parsed" (seen !! 1)
                  assertNotContains "violated required properties" (seen !! 1)
              , testCase "JSON decode error exhausts retries" $ do
                  responses <- newIORef (repeat "not valid json")
                  seenContexts <- newIORef ([] :: [Text])
                  let cfg = SeqMockConfig responses seenContexts
                  result <-
                    Eff.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 1}) $
                      Eff.prompt @User
                  case result of
                    Left err -> assertContains "JSON decode error" err
                    Right _ -> fail "Expected Left but got Right"
                  seen <- readIORef seenContexts
                  -- 1 original + 1 retry = 2 total calls
                  length seen @?= 2
              , testCase "validation always fails — error after retries exhausted (1 + maxRetries calls)" $ do
                  responses <- newIORef (repeat "{\"userName\":\"Alice\",\"userEmail\":\"\"}")
                  seenContexts <- newIORef ([] :: [Text])
                  let cfg = SeqMockConfig responses seenContexts
                  result <-
                    Eff.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) $
                      Eff.prompt @User
                  case result of
                    Left err -> assertContains "The email address is not empty." err
                    Right _ -> assertFailure "Expected Left but got Right"
                  seen <- readIORef seenContexts
                  length seen @?= 3
              ]
          , testGroup
              "promptPar / promptsParallel genuinely run concurrently"
              [ testCase "promptPar: two branches meet at a barrier — sequential execution would deadlock" $ do
                  entered <- newMVar (0 :: Int)
                  outcome <-
                    timeout (5 * 1000 * 1000) $
                      Eff.runPromptResultEff (barrierBackend entered 2) defaultPromptConfig $
                        Eff.promptPar (Eff.prompt @Counter) (Eff.prompt @Counter)
                  case outcome of
                    Nothing ->
                      assertFailure
                        "promptPar branches never both entered the backend within 5s: they did not run concurrently"
                    Just (Left err) -> assertFailure (show err)
                    Just (Right _) -> pure ()
              , testCase "promptsParallel: three branches meet at a barrier" $ do
                  entered <- newMVar (0 :: Int)
                  outcome <-
                    timeout (5 * 1000 * 1000) $
                      Eff.runPromptResultEff (barrierBackend entered 3) defaultPromptConfig $
                        Eff.promptsParallel (replicate 3 (Eff.prompt @Counter))
                  case outcome of
                    Nothing ->
                      assertFailure
                        "promptsParallel branches never all entered the backend within 5s: they did not run concurrently"
                    Just (Left err) -> assertFailure (show err)
                    Just (Right xs) -> length xs @?= 3
              ]
          ]
      , testGroup
          "Tools as effects"
          [ testCase "toolBinding + promptTools + runTool: dispatch reaches the registered handler" $ do
              seenInput <- newIORef Nothing
              let handler = ToolHandler $ \q -> do
                    writeIORef seenInput (Just q)
                    pure (Right "fetched page text")
                  program :: Eff (Tool WebFetch : BaseEs) Counter
                  program = Eff.promptTools [toolBinding @WebFetch]
              result <-
                Eff.runPromptResultEff
                  (toolCallBackend (toolName (Proxy @WebFetch)) (toJSON (WebFetch "https://example.com")))
                  defaultPromptConfig
                  (runTool handler program)
              case result of
                Right (Counter 0) -> pure ()
                Right (Counter n) -> assertFailure ("Expected Counter 0, got Counter " <> show n)
                Left err -> assertFailure (show err)
              seen <- readIORef seenInput
              seen @?= Just (WebFetch "https://example.com")
          , testCase "sub-program runs with a strict subset of the caller's tools" $ do
              let ddgHandler = ToolHandler $ \_ -> pure (Right "ddg result")
                  wikiHandler = ToolHandler $ \_ -> pure (Right "wiki result")
              result <-
                Eff.runPromptResultEff
                  (toolCallBackend (toolName (Proxy @DuckDuckGoSearch)) (toJSON (DuckDuckGoSearch "x")))
                  defaultPromptConfig
                  (runTool wikiHandler (runTool ddgHandler callerProgram))
              case result of
                Right (Counter 0) -> pure ()
                Right (Counter n) -> assertFailure ("Expected Counter 0, got Counter " <> show n)
                Left err -> assertFailure (show err)
          , testCase "tool budget: successful calls cost exactly the budget, then one final tool-free call" $ do
              callCount <- newIORef (0 :: Int)
              roundRef <- newIORef (0 :: Int)
              let handler = ToolHandler $ \_ -> do
                    modifyIORef' callCount (+ 1)
                    pure (Right "ok")
                  program :: Eff (Tool DuckDuckGoSearch : BaseEs) Counter
                  program = Eff.promptTools [toolBinding @DuckDuckGoSearch]
              result <-
                Eff.runPromptResultEff
                  (roundBasedToolBackend (toolName (Proxy @DuckDuckGoSearch)) 5 roundRef)
                  (defaultPromptConfig {maxToolSteps = Just 2})
                  (runTool handler program)
              case result of
                Right (Counter 0) -> pure ()
                Right (Counter n) -> assertFailure ("Expected Counter 0, got Counter " <> show n)
                Left err -> assertFailure (show err)
              n <- readIORef callCount
              n @?= 2
          , testCase "tool budget: failed calls are free and do not consume it" $ do
              roundRef <- newIORef (0 :: Int)
              let handler = ToolHandler $ \_ -> pure (Left "nope")
                  program :: Eff (Tool DuckDuckGoSearch : BaseEs) Counter
                  program = Eff.promptTools [toolBinding @DuckDuckGoSearch]
              result <-
                Eff.runPromptResultEff
                  (roundBasedToolBackend (toolName (Proxy @DuckDuckGoSearch)) 3 roundRef)
                  (defaultPromptConfig {maxToolSteps = Just 1})
                  (runTool handler program)
              case result of
                Right (Counter 0) -> pure ()
                Right (Counter n) -> assertFailure ("Expected Counter 0, got Counter " <> show n)
                Left err ->
                  assertFailure
                    ( "Expected the natural finish after 3 failed calls on a budget of 1, but got: "
                        <> show err
                    )
              {- 'roundBasedToolBackend' bumps 'roundRef' once per
              'runBackendChat' call regardless of outcome, so its count is
              the tell: with the budget never actually spent, all 3
              attempted (and failed) rounds plus the final answer round
              reach 'runBackendChat', for 4 in total. Were failed calls to
              consume budget instead, a budget of 1 would trip the
              exhaustion branch after round 2, and 'runBackendChat' would
              never be reached again — this assertion is what would catch
              that, since both paths otherwise settle on the same
              'Counter 0'.
              -}
              n <- readIORef roundRef
              n @?= 4
          ]
      , testGroup
          "Web tools"
          [ testCase "webFetchHandler: timeout interrupts a request to a server that never answers" $
              withSilentServer $ \port -> do
                let ToolHandler fetch = webFetchHandler
                start <- getMonotonicTime
                -- If the handler swallowed the 'timeout''s asynchronous
                -- exception it would hand back @Just (Left "WebFetch error:
                -- ...")@ here instead of 'Nothing'.
                result <- timeout 1_000_000 (fetch (WebFetch ("http://127.0.0.1:" <> T.pack (show port))))
                elapsed <- subtract start <$> getMonotonicTime
                case result of
                  Nothing -> pure ()
                  Just r -> assertFailure ("expected the timeout to fire, but the handler returned: " <> show r)
                assertBool ("timeout took " <> show elapsed <> "s, expected well under 3s") (elapsed < 3)
          , testCase "webFetchHandler: a synchronous HTTP failure still comes back as a tool error" $ do
              -- The listener is gone once 'withSilentServer' returns, so
              -- its port refuses connections.
              port <- withSilentServer pure
              let ToolHandler fetch = webFetchHandler
              result <- timeout 5_000_000 (fetch (WebFetch ("http://127.0.0.1:" <> T.pack (show port))))
              case result of
                Just (Left err) -> assertBool ("unexpected error text: " <> show err) ("WebFetch error: " `T.isPrefixOf` err)
                other -> assertFailure ("expected a WebFetch tool error, got: " <> show other)
          ]
      ]
