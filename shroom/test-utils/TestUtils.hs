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
import Control.Monad.Prompt
import Control.Monad.Prompt.Backend (Backend (..), BackendError (..), BackendReply (..))

-- * Mock backends

{- | A mock 'LLMBackend' that records the context passed to each call
(as flat text via 'renderContextItems') and always returns @0@ (encoded
as JSON) for 'Counter'.
-}
newtype MockConfig = MockConfig (IORef [Text])

instance LLMBackend MockConfig where
  runChatWithTools (MockConfig ref) _promptCfg ctx _typeDesc _schema _toolDefs _dispatch _maxToolSteps = liftIO $ do
    atomicModifyIORef' ref (\xs -> (xs <> [renderContextItems ctx], ()))
    -- Return a valid Counter JSON (the newtype wraps an Int)
    pure $ Right (toStrict (decodeUtf8 (encode (0 :: Int))))

{- | 'Backend' for 'MockConfig', built directly on 'runChat'. No tool
concept: the offered tools are ignored and every reply is a 'BackendAnswer'.
-}
mockBackend :: (MonadIO m) => MockConfig -> Backend m
mockBackend cfg =
  Backend $ \ctx typeDesc schema _toolDefs ->
    either (Left . BackendTransportError) (Right . BackendAnswer) <$> runChat cfg ctx typeDesc schema

{- | A mock 'LLMBackend' that returns responses from a list in order,
repeating the last one when the list is exhausted.
Records contexts as flat text via 'renderContextItems'.
-}
data SeqMockConfig = SeqMockConfig (IORef [Text]) (IORef [Text])

instance LLMBackend SeqMockConfig where
  runChatWithTools (SeqMockConfig responses seenCtxs) _promptCfg ctx _typeDesc _schema _toolDefs _dispatch _maxToolSteps = liftIO $ do
    atomicModifyIORef' seenCtxs (\xs -> (xs <> [renderContextItems ctx], ()))
    atomicModifyIORef' responses $ \case
      [] -> ([], Left ("SeqMockConfig: no more responses" :: Text))
      [x] -> ([x], Right x)
      (x : rest) -> (rest, Right x)

{- | 'Backend' for 'SeqMockConfig', built directly on 'runChat'. No tool
concept: the offered tools are ignored and every reply is a 'BackendAnswer'.
-}
seqMockBackend :: (MonadIO m) => SeqMockConfig -> Backend m
seqMockBackend cfg =
  Backend $ \ctx typeDesc schema _toolDefs ->
    either (Left . BackendTransportError) (Right . BackendAnswer) <$> runChat cfg ctx typeDesc schema

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

-- | A mock 'LLMBackend' that always returns a backend-level error.
data InjectFailConfig = InjectFailConfig

instance LLMBackend InjectFailConfig where
  runChatWithTools _ _ _ _ _ _ _ _ = pure (Left "injected failure")

-- | 'Backend' for 'InjectFailConfig': always fails, unconditionally.
injectFailBackend :: (Applicative m) => InjectFailConfig -> Backend m
injectFailBackend _ = Backend $ \_ _ _ _ -> pure (Left (BackendTransportError "injected failure"))
