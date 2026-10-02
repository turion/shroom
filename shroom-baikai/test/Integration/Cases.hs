{- | The one shared list of backend-integration test cases: written once
against "Control.Monad.Prompt.Backend"'s interface, then run in
"Integration.Claude" (against 'Control.Monad.Prompt.Baikai.claudeBackend')
and twice more in "Integration.Ollama" — against
'Control.Monad.Prompt.Baikai.localOllamaBackend', and against
'Control.Monad.Prompt.Baikai.openAICompatBackend' itself, called with the
same base URL and an explicit placeholder key rather than
'localOllamaBackend'\'s own 'Nothing' — the front-door path a caller
supplying a real key would exercise. The three call sites differ only in
which 'Backend' they pass in, never in the cases themselves.

Covers at least the union of what "Integration.Claude" and
"Integration.Ollama" each covered before this module existed: a typed value
comes back ('User', 'Counter'), a property-validated chain runs end to end
('conferenceChain'), and a tool actually gets dispatched and its result read
back in ('conferenceChainWithTools'). Each suite keeps whatever
provider-specific cases do not fit this shape (web-search-tool smoke tests
and failure reporting in "Integration.Claude"; 'celebrityChain' and the
non-object top-level response types in "Integration.Ollama") alongside a
call to 'backendIntegrationTests'.

'conferenceChain'\'s assertions here are deliberately looser than
"Integration.Ollama" checked on its own before this module existed: the
upper bound on speaker count (@n <= 10@) encoded an assumption about a weak
local model's typical output size, not a property every provider must
satisfy, so it is dropped from the shared list rather than risking a
spurious failure against a provider that reasons about more speakers. The
slot-continuity check (each slot picks up exactly where the last one left
off) is a genuine property of a valid schedule regardless of provider, so
it stays.
-}
module Integration.Cases (backendIntegrationTests, sharedPromptConfig) where

-- base
import Data.IORef

-- effectful
import Effectful (Eff, IOE)
import Effectful.Concurrent.Async (Concurrent)
import Effectful.Error.Static (Error)
import Effectful.State.Static.Local (State)

-- text
import Data.Text (Text)

-- tasty
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt.Backend (Backend, ContextItem)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.Tool (ToolHandler (..), runTool, runToolHandler, toolBinding)
import Control.Monad.Prompt.Tool.Web (DuckDuckGoSearch, duckDuckGoSearchHandler)

-- test
import ConferenceTypes
import Types

{- | Generous enough for the weakest provider exercised here (a small local
Ollama model) without weakening any assertion for a stronger one: more
retries and tool steps only widen the budget before a call is given up on,
they do not change what counts as success.
-}
sharedPromptConfig :: PromptConfig
sharedPromptConfig = defaultPromptConfig {maxRetries = 10, maxToolSteps = Just 10}

-- | The concrete effect stack every call site here runs its program through.
type BackendEs = Eff '[State [ContextItem], Error Text, Concurrent, IOE]

{- | Run the shared cases — a typed value, a property-validated chain, and a
tool-dispatch case proving the tool was actually invoked — against one
'Backend'.
-}
backendIntegrationTests :: Backend BackendEs -> TestTree
backendIntegrationTests backend =
  testGroup
    "backend integration cases"
    [ testCase "prompt returns a User" $ do
        result <- Effect.runPromptResultEff backend sharedPromptConfig $ do
          Effect.context "Return a JSON object for a user named Alice with email alice@example.com"
          Effect.prompt @User
        case result of
          Left err -> assertFailure (show err)
          Right user -> userName user @?= "Alice"
    , testCase "prompt returns a Counter" $ do
        result <- Effect.runPromptResultEff backend sharedPromptConfig $ do
          Effect.context "Return a JSON counter object with value 42."
          Effect.prompt @Counter
        case result of
          Left err -> assertFailure (show err)
          Right (Counter n) -> n @?= 42
    , testCase "conferenceChain produces a valid schedule" $ do
        result <- Effect.runPromptResultEff backend sharedPromptConfig conferenceChain
        case result of
          Left err -> assertFailure (show err)
          Right (allSpeakers, talks, schedule) -> do
            let n = length (speakers allSpeakers)
            assertBool "at least 3 speakers" (n >= 3)
            length talks @?= n
            let slots = scheduleSlots schedule
            assertBool "at least one slot" (not (null slots))
            let pairs = zip slots (drop 1 slots)
            assertBool "each slot picks up where the last left off" (all (\(a, b) -> slotEnd a == slotStart b) pairs)
    , testCase "conferenceChainWithTools: web_search tool is invoked at least once" $ do
        callCount <- newIORef (0 :: Int)
        let countingHandler = ToolHandler $ \q -> do
              atomicModifyIORef' callCount (\n -> (n + 1, ()))
              runToolHandler duckDuckGoSearchHandler q
        result <-
          Effect.runPromptResultEff backend sharedPromptConfig $
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
