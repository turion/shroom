module Main (main) where

-- base
import System.Environment (lookupEnv)

-- aeson
import Data.Aeson (object)

-- text
import Data.Text (Text, pack)
import Data.Text qualified as T

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, testCaseSteps)

-- shroom
import Control.Monad.Prompt.Backend (Backend (..), BackendReply (..), ContextItem (..))
import Control.Monad.Prompt.Baikai (anthropicCompatBackend, localOllamaBackend, normaliseOllamaHost, openAICompatBackend)
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect
import Control.Monad.Prompt.Tool (runTool, toolBinding)
import Control.Monad.Prompt.Tool.Web (DuckDuckGoSearch, WebFetch, duckDuckGoSearchHandler, webFetchHandler)

-- test
import CelebrityTypes
import Integration.Cases (backendIntegrationTests)
import ScalarTypes (ScalarInt (..), ScalarIntList (..), ScalarText (..))

main :: IO ()
main = do
  mModel <- lookupEnv "OLLAMA_MODEL"
  -- Falls back to the same tag 'nix/ollama-shroom.nix' pulls by default, so running
  -- this suite locally against a module-provisioned Ollama without setting
  -- OLLAMA_MODEL asks for a model that is actually there. Keep the two in agreement
  -- by hand; a Haskell test suite has no business parsing a nix file at runtime.
  let model = maybe "llama3.2:1b" pack mModel
  ollamaIntegrationTests model >>= defaultMain

{- | 'localOllamaBackend' honours @OLLAMA_HOST@ itself, so this suite no
longer reads it directly the way it once did, back when it built the old
@Control.Monad.Prompt.Ollama@ module's @defaultOllamaBackendConfig@ — both
deleted along with that module.

The second backend built here calls 'Control.Monad.Prompt.Baikai.openAICompatBackend'
itself, the front-door constructor, rather than going around it: the same
@OLLAMA_HOST@-derived base URL 'localOllamaBackend' passes through
underneath, but with an explicit placeholder key where 'localOllamaBackend'
always passes 'Nothing' — exercising the constructor's @Just@ branch, which
'localOllamaBackend' never reaches. Ollama itself checks no credential, so
the placeholder only has to be non-empty, never correct.

The third calls 'Control.Monad.Prompt.Baikai.anthropicCompatBackend' against the
same host, which Ollama also serves as an Anthropic Messages API (@\/v1\/messages@).
Its one case sends a plain request through 'runBackendChat' and asserts that a
reply came back and was parsed as an Anthropic message, so it covers the
Anthropic Messages wire format and @baikai-claude@'s envelope parsing through
shroom's adapter, without a paid key. It does /not/ run the shared typed
'backendIntegrationTests' cases: Ollama's @\/v1\/messages@ ignores the
structured-output schema and answers in prose, so typed prompting on the
Anthropic path is covered only by the local Claude suite ("Integration.Claude").
Nor does it cover 'Control.Monad.Prompt.Baikai.claudeBackend'\'s fixed
@api.anthropic.com@ URL.
-}
ollamaIntegrationTests :: Text -> IO TestTree
ollamaIntegrationTests model = do
  localBackend <- localOllamaBackend model
  mHost <- lookupEnv "OLLAMA_HOST"
  let baseUrl = maybe "http://127.0.0.1:11434" (normaliseOllamaHost . T.pack) mHost
  genericBackend <- openAICompatBackend baseUrl model (Just "unused-ollama-checks-no-credential")
  anthropicBackend <- anthropicCompatBackend baseUrl model "unused-ollama-checks-no-credential"
  pure $
    testGroup
      "Ollama integration"
      [ testGroup
          "localOllamaBackend"
          [ backendIntegrationTests localBackend
          , testCaseSteps "celebrityChain: chain completes successfully with tools available" $ \step -> do
              let pcfg =
                    defaultPromptConfig
                      { maxRetries = 5
                      , maxToolSteps = Just 10
                      , debugLog = Just (step . T.unpack)
                      }
              result <-
                Effect.runPromptResultEff localBackend pcfg $
                  runTool duckDuckGoSearchHandler $
                    runTool webFetchHandler $
                      celebrityChain [toolBinding @DuckDuckGoSearch, toolBinding @WebFetch]
              case result of
                Left err -> assertFailure (show err)
                Right fact -> do
                  assertBool "celebrity not empty" (not (T.null (celebrity fact)))
                  assertBool "trivia fact not empty" (not (T.null (triviaFact fact)))
          , testCase "prompt returns a non-object Int" $ do
              result <- Effect.runPromptResultEff localBackend (defaultPromptConfig {maxRetries = 10}) $ do
                Effect.context "How many legs does a spider have? Answer with just the number."
                Effect.prompt @ScalarInt
              case result of
                Left err -> assertFailure (show err)
                -- Any 'Int' proves the non-object top-level type round-tripped;
                -- see "Integration.Cases" for why the answer is not asserted.
                Right (ScalarInt _) -> pure ()
          , testCase "prompt returns a non-object Text" $ do
              result <- Effect.runPromptResultEff localBackend (defaultPromptConfig {maxRetries = 10}) $ do
                Effect.context "Return the word \"hello\" and nothing else."
                Effect.prompt @ScalarText
              case result of
                Left err -> assertFailure (show err)
                Right (ScalarText t) -> assertBool "non-empty" (not (T.null t))
          , testCase "prompt returns a non-object array of Int" $ do
              result <- Effect.runPromptResultEff localBackend (defaultPromptConfig {maxRetries = 10}) $ do
                Effect.context "Return the first three positive integers as a JSON array, in order."
                Effect.prompt @ScalarIntList
              case result of
                Left err -> assertFailure (show err)
                -- Any list of 'Int' proves the non-object array round-tripped;
                -- the values are the model's to get right, not shroom's.
                Right (ScalarIntList _) -> pure ()
          ]
      , testGroup
          "openAICompatBackend (explicit base URL, explicit key)"
          [backendIntegrationTests genericBackend]
      , testGroup
          "anthropicCompatBackend (Ollama's Anthropic Messages API)"
          [ testCase "a plain request comes back as a parsed Anthropic reply" $ do
              -- Transport only: Ollama's @\/v1\/messages@ ignores the schema, so
              -- all this asserts is that a reply arrived, was read as an
              -- Anthropic message and carries some text.
              result <-
                runBackendChat
                  anthropicBackend
                  [UserMessage "Say hello."]
                  "Reply with one short sentence."
                  (object [])
                  []
              case result of
                Left err -> assertFailure (show err)
                Right (BackendAnswer t) -> assertBool "non-empty reply" (not (T.null (T.strip t)))
                Right reply -> assertFailure ("expected an answer, got: " <> show reply)
          ]
      ]
