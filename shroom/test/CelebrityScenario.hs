module Main (main) where

-- base
import Control.Monad (void)
import Data.IORef

-- text
import Data.Text (Text)
import Data.Text qualified as T

-- tasty
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

-- shroom
import Control.Monad.Prompt.Effect (PromptConfig (..), defaultPromptConfig)
import Control.Monad.Prompt.Effect qualified as Effect

-- test
import CelebrityTypes
import TestUtils

-- | A valid CelebrityList JSON with exactly 3 celebrities.
listJson :: Text
listJson =
  "{\"celebrities\":[\"Albert Einstein\",\"Marie Curie\",\"Nikola Tesla\"]}"

-- | A valid CelebrityFact JSON.
factJson :: Text
factJson =
  "{\"celebrity\":\"Marie Curie\",\"triviaFact\":\"She was the first woman to win a Nobel Prize, and the only person to win Nobel Prizes in two different sciences (Physics and Chemistry).\"}"

defaultResponses :: [Text]
defaultResponses = [listJson, factJson]

-- | Run celebrityChain with the given canned responses; return (result, seen contexts).
runCelebrity :: PromptConfig -> [Text] -> IO (Either Text CelebrityFact, [Text])
runCelebrity cfg responseList = do
  responses <- newIORef responseList
  seenCtxs <- newIORef []
  result <- Effect.runPromptResultEff (seqMockBackend (SeqMockConfig responses seenCtxs)) cfg (celebrityChain [])
  seen <- readIORef seenCtxs
  pure (result, seen)

-- | Like 'runCelebrity' but fails the test on 'Left'.
runCelebrityOk :: PromptConfig -> [Text] -> IO (CelebrityFact, [Text])
runCelebrityOk cfg responseList = do
  (result, seen) <- runCelebrity cfg responseList
  case result of
    Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
    Right x -> pure (x, seen)

main :: IO ()
main =
  defaultMain $
    testGroup
      "celebrity scenario"
      [ testCase "chain completes successfully" $ do
          void $ runCelebrityOk defaultPromptConfig defaultResponses
      , testCase "returned celebrity name is non-empty" $ do
          (fact, _) <- runCelebrityOk defaultPromptConfig defaultResponses
          not (T.null (celebrity fact)) @?= True
      , testCase "returned trivia fact is non-empty" $ do
          (fact, _) <- runCelebrityOk defaultPromptConfig defaultResponses
          not (T.null (triviaFact fact)) @?= True
      , testCase "exactly 2 LLM calls are made (1 list + 1 fact)" $ do
          (_, seen) <- runCelebrity defaultPromptConfig defaultResponses
          length seen @?= 2
      , testCase "second call context includes the chosen celebrity name" $ do
          (_, seen) <- runCelebrity defaultPromptConfig defaultResponses
          -- The chosen celebrity is names !! (3 `mod` 3) = names !! 0 = "Albert Einstein"
          assertContains "Albert Einstein" (seen !! 1)
      , testCase "empty trivia fact triggers retry" $ do
          let badFact = "{\"celebrity\":\"Marie Curie\",\"triviaFact\":\"\"}"
          responses <- newIORef [listJson, badFact, factJson]
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <-
            Effect.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) (celebrityChain [])
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right fact -> not (T.null (triviaFact fact)) @?= True
          seen <- readIORef seenCtxs
          -- 1 list + 1 bad fact + 1 retry fact = 3 calls
          length seen @?= 3
          assertContains "IMPORTANT" (seen !! 2)
      , testCase "list with wrong count triggers retry" $ do
          let badList = "{\"celebrities\":[\"Only One Celebrity\"]}"
          responses <- newIORef [badList, listJson, factJson]
          seenCtxs <- newIORef ([] :: [Text])
          let cfg = SeqMockConfig responses seenCtxs
          result <-
            Effect.runPromptResultEff (seqMockBackend cfg) (defaultPromptConfig {maxRetries = 2}) (celebrityChain [])
          case result of
            Left err -> fail $ "Expected Right but got Left: " <> T.unpack err
            Right _ -> pure ()
          seen <- readIORef seenCtxs
          -- 1 bad list + 1 good list + 1 fact = 3 calls
          length seen @?= 3
          assertContains "IMPORTANT" (seen !! 1)
          assertContains "exactly 3" (seen !! 1)
      ]
