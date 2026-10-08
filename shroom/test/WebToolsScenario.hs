module Main (main) where

-- text
import Data.Text qualified as T

-- tasty
import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- shroom
import Control.Monad.Prompt.Tool (runToolHandler)
import Control.Monad.Prompt.Tool.Web (
  DuckDuckGoSearch (..),
  WebFetch (..),
  WikipediaSearch (..),
  duckDuckGoSearchHandler,
  webFetchHandler,
  wikipediaSearchHandler,
 )

main :: IO ()
main =
  defaultMain $
    testGroup
      "web tools"
      [ testGroup
          "duckDuckGoSearch"
          [ testCase "named entity returns a non-empty result" $ do
              result <- runToolHandler duckDuckGoSearchHandler (DuckDuckGoSearch "Marie Curie")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "result should be non-empty" (not (T.null txt))
          , testCase "named entity result includes URL" $ do
              result <- runToolHandler duckDuckGoSearchHandler (DuckDuckGoSearch "Marie Curie")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "result should contain a URL" ("URL: " `T.isInfixOf` txt)
          , testCase "vague query returns 'No result found'" $ do
              result <- runToolHandler duckDuckGoSearchHandler (DuckDuckGoSearch "xyzzy frob quux 12345")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "vague query should say No result found" ("No result found" `T.isInfixOf` txt)
          ]
      , testGroup
          "wikipediaSearch"
          [ testCase "query returns non-empty results" $ do
              result <- runToolHandler wikipediaSearchHandler (WikipediaSearch "Marie Curie")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "result should be non-empty" (not (T.null txt))
          , testCase "result contains Wikipedia URL" $ do
              result <- runToolHandler wikipediaSearchHandler (WikipediaSearch "Marie Curie")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "result should contain wikipedia.org URL" ("wikipedia.org" `T.isInfixOf` txt)
          , testCase "vague query still returns results (Wikipedia is broad)" $ do
              result <- runToolHandler wikipediaSearchHandler (WikipediaSearch "celebrity trivia")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "Wikipedia should find something" (not (T.null txt))
          ]
      , testGroup
          "webFetch"
          [ testCase "fetching a known URL returns non-empty text" $ do
              result <- runToolHandler webFetchHandler (WebFetch "https://en.wikipedia.org/wiki/Marie_Curie")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "fetched content should be non-empty" (not (T.null txt))
          , testCase "fetched content is truncated to 2000 chars" $ do
              result <- runToolHandler webFetchHandler (WebFetch "https://en.wikipedia.org/wiki/Marie_Curie")
              case result of
                Left err -> assertFailure ("Expected Right but got Left: " <> T.unpack err)
                Right txt -> assertBool "content should be at most 2000 chars" (T.length txt <= 2000)
          , testCase "invalid URL returns Left" $ do
              result <- runToolHandler webFetchHandler (WebFetch "not-a-url")
              case result of
                Left _ -> pure ()
                Right _ -> assertFailure "Expected Left for invalid URL"
          ]
      ]
