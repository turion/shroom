{-# LANGUAGE LambdaCase #-}

module TestUtils (
  MockConfig (..),
  SeqMockConfig (..),
  InjectFailConfig (..),
  mockBackend,
  seqMockBackend,
  injectFailBackend,
  assertContains,
  assertNotContains,
  textIsInfixOf,
) where

-- base

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef

-- aeson
import Data.Aeson (encode)

-- text
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)

-- shroom
import Control.Monad.Prompt.Backend (Backend (..), BackendError (..), BackendReply (..), renderContextItems)

-- * Mock backends

{- | A mock 'Backend' that records the context passed to each call (as flat
text via 'renderContextItems') and always returns @0@ (encoded as JSON) for
'Counter'. No tool concept: the offered tools are ignored and every reply
is a 'BackendAnswer'.
-}
newtype MockConfig = MockConfig (IORef [Text])

mockBackend :: (MonadIO m) => MockConfig -> Backend m
mockBackend (MockConfig ref) =
  Backend $ \ctx _typeDesc _schema _toolDefs -> liftIO $ do
    atomicModifyIORef' ref (\xs -> (xs <> [renderContextItems ctx], ()))
    -- Return a valid Counter JSON (the newtype wraps an Int)
    pure $ Right (BackendAnswer (toStrict (decodeUtf8 (encode (0 :: Int)))))

{- | A mock 'Backend' that returns responses from a list in order, repeating
the last one when the list is exhausted. Records contexts as flat text via
'renderContextItems'. No tool concept: the offered tools are ignored and
every reply is a 'BackendAnswer'.
-}
data SeqMockConfig = SeqMockConfig (IORef [Text]) (IORef [Text])

seqMockBackend :: (MonadIO m) => SeqMockConfig -> Backend m
seqMockBackend (SeqMockConfig responses seenCtxs) =
  Backend $ \ctx _typeDesc _schema _toolDefs -> liftIO $ do
    atomicModifyIORef' seenCtxs (\xs -> (xs <> [renderContextItems ctx], ()))
    atomicModifyIORef' responses $ \case
      [] -> ([], Left (BackendTransportError "SeqMockConfig: no more responses"))
      [x] -> ([x], Right (BackendAnswer x))
      (x : rest) -> (rest, Right (BackendAnswer x))

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

-- | A marker config for a mock 'Backend' that always returns a backend-level error.
data InjectFailConfig = InjectFailConfig

-- | 'Backend' for 'InjectFailConfig': always fails, unconditionally.
injectFailBackend :: (Applicative m) => InjectFailConfig -> Backend m
injectFailBackend _ = Backend $ \_ _ _ _ -> pure (Left (BackendTransportError "injected failure"))
