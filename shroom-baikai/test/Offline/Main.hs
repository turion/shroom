module Main (main) where

-- base
import Control.Concurrent (forkIO, killThread, newEmptyMVar, takeMVar)
import Control.Exception (bracket)
import Control.Monad (void)
import GHC.Clock (getMonotonicTime)
import System.Timeout (timeout)

-- aeson
import Data.Aeson (object)

-- text
import Data.Text qualified as T

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

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- shroom
import Control.Monad.Prompt.Backend (Backend (..))
import Control.Monad.Prompt.Baikai (openAICompatBackend)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "shroom-baikai offline"
    [ testCase "timeout interrupts a baikaiBackend call against a server that never answers" $
        withSilentServer $ \port -> do
          backend <- openAICompatBackend ("http://127.0.0.1:" <> T.pack (show port)) "unused" (Just "unused")
          start <- getMonotonicTime
          -- If the adapter swallowed the 'timeout''s asynchronous exception it
          -- would hand back @Just (Left (BackendTransportError _))@ here
          -- instead of 'Nothing'.
          result <- timeout 1_000_000 (runBackendChat backend [] "a type" (object []) [])
          elapsed <- subtract start <$> getMonotonicTime
          case result of
            Nothing -> pure ()
            Just r -> assertFailure ("expected the timeout to fire, but the call returned: " <> show r)
          assertBool ("timeout took " <> show elapsed <> "s, expected well under 3s") (elapsed < 3)
    ]

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
