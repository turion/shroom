module TestUtils (
  MockConfig (..),
  SeqMockConfig (..),
  assertContains,
  assertNotContains,
  textIsInfixOf,
) where

-- base

import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- aeson
import Data.Aeson (encode)

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- shroom
import Control.Monad.Prompt

-- * Mock backends

{- | A mock 'LLMBackend' that records the context passed to each call
and always returns @0@ (encoded as JSON) for 'Counter'.
-}
newtype MockConfig = MockConfig (IORef [Text])

instance LLMBackend MockConfig where
  runChat (MockConfig ref) ctx _typeDesc _schema = liftIO $ do
    atomicModifyIORef' ref (\xs -> (xs <> [ctx], ()))
    -- Return a valid Counter JSON (the newtype wraps an Int)
    pure $ Right (toStrict (decodeUtf8 (encode (0 :: Int))))

{- | A mock 'LLMBackend' that returns responses from a list in order,
repeating the last one when the list is exhausted.
-}
data SeqMockConfig = SeqMockConfig (IORef [Text]) (IORef [Text])

instance LLMBackend SeqMockConfig where
  runChat (SeqMockConfig responses seenCtxs) ctx _typeDesc _schema = liftIO $ do
    atomicModifyIORef' seenCtxs (\xs -> (xs <> [ctx], ()))
    atomicModifyIORef' responses $ \rs -> case rs of
      [] -> ([], Left "SeqMockConfig: no more responses")
      [x] -> ([x], Right x)
      (x : rest) -> (rest, Right x)

-- * Helpers

assertContains :: Text -> Text -> IO ()
assertContains needle haystack
  | needle `textIsInfixOf` haystack = pure ()
  | otherwise = fail $ "Expected " <> show needle <> " in:\n" <> show haystack

assertNotContains :: Text -> Text -> IO ()
assertNotContains needle haystack
  | needle `textIsInfixOf` haystack =
      fail $ "Expected " <> show needle <> " to NOT be in:\n" <> show haystack
  | otherwise = pure ()

textIsInfixOf :: Text -> Text -> Bool
textIsInfixOf = T.isInfixOf
