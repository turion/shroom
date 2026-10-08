module Main (main) where

-- base
import Control.Concurrent (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket, finally)
import Control.Monad (void)
import Data.Char (isAsciiUpper)
import GHC.Clock (getMonotonicTime)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Timeout (timeout)

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL

-- aeson
import Data.Aeson (Value (Null, Number, Object), decodeStrict, encode, object, (.=))
import Data.Aeson.KeyMap qualified as KeyMap

-- text
import Data.Text qualified as T

-- network
import Network.Socket (
  Family (AF_INET),
  PortNumber,
  SockAddr (SockAddrInet),
  Socket,
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
import Network.Socket.ByteString (recv, sendAll)

-- tasty
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

-- shroom
import Control.Monad.Prompt.Backend (Backend (..), BackendError (..))
import Control.Monad.Prompt.Baikai (localOllamaBackend, openAICompatBackend)

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
    , testCase "openAICompatBackend caps the output with max_tokens, which Ollama honours" $
        withRecordingServer $ \port recorded -> do
          backend <- openAICompatBackend ("http://127.0.0.1:" <> T.pack (show port)) "unused" (Just "unused")
          void (timeout 5_000_000 (runBackendChat backend [] "a type" (object []) []))
          recorded >>= assertTokenCap
    , testCase "localOllamaBackend caps the output with max_tokens, which Ollama honours" $
        withRecordingServer $ \port recorded ->
          withEnv "OLLAMA_HOST" ("127.0.0.1:" <> show port) $ do
            backend <- localOllamaBackend "unused"
            void (timeout 5_000_000 (runBackendChat backend [] "a type" (object []) []))
            recorded >>= assertTokenCap
    , testCase "a finish_reason of length is a truncated reply, not an answer that fails to parse" $
        withLengthStopServer "{\"userName\":\"Ali" $ \port -> do
          backend <- openAICompatBackend ("http://127.0.0.1:" <> T.pack (show port)) "unused" (Just "unused")
          result <- timeout 5_000_000 (runBackendChat backend [] "a type" (object []) [])
          result @?= Just (Left (BackendTruncated "{\"userName\":\"Ali"))
    ]

{- | The request body names the cap @max_tokens@ — the only spelling Ollama's
OpenAI-compatible layer reads — and no longer carries the
@max_completion_tokens@ it ignores.
-}
assertTokenCap :: ByteString -> IO ()
assertTokenCap body = case decodeStrict body of
  Just (Object o) -> do
    KeyMap.lookup "max_tokens" o @?= Just (Number 4096)
    KeyMap.lookup "max_completion_tokens" o @?= Nothing
  other -> assertFailure ("expected a JSON object request body, got: " <> show other <> " from " <> show body)

-- | Run an action with an environment variable set, restoring its old value (or absence) afterwards.
withEnv :: String -> String -> IO a -> IO a
withEnv name value act = do
  old <- lookupEnv name
  setEnv name value
  act `finally` maybe (unsetEnv name) (setEnv name) old

{- | Run an action against a localhost TCP listener on a free port. Binds port
0, so the OS picks a free one and parallel runs cannot collide; localhost
only, no external service.
-}
withListener :: (Socket -> PortNumber -> IO a) -> IO a
withListener act =
  bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen listener 1
    port <-
      getSocketName listener >>= \case
        SockAddrInet p _ -> pure p
        other -> fail ("unexpected listener address: " <> show other)
    act listener port

{- | Run an action against a localhost TCP listener that accepts a connection
and then never reads from it or writes to it.
-}
withSilentServer :: (PortNumber -> IO a) -> IO a
withSilentServer act =
  withListener $ \listener port -> do
    -- Hold the accepted connection open, silent, until the thread is killed.
    blocked <- newEmptyMVar @()
    bracket
      ( forkIO . void $ do
          (conn, _) <- accept listener
          bracket (pure conn) close (\_ -> takeMVar blocked)
      )
      killThread
      (\_ -> act port)

{- | Run an action against a localhost HTTP server that records the body of
the first request it receives and answers it with a 400, so the client
gives up at once instead of retrying. The action gets the port and an
action that waits (up to five seconds) for the recorded body.
-}
withRecordingServer :: (PortNumber -> IO ByteString -> IO a) -> IO a
withRecordingServer act =
  withListener $ \listener port -> do
    bodyVar <- newEmptyMVar
    bracket
      ( forkIO . void $ do
          (conn, _) <- accept listener
          bracket (pure conn) close $ \c -> do
            readRequestBody c >>= putMVar bodyVar
            let reply = "{\"error\":{\"message\":\"recording server\"}}"
            sendAll c $
              "HTTP/1.1 400 Bad Request\r\nContent-Type: application/json\r\nConnection: close\r\nContent-Length: "
                <> BC.pack (show (BS.length reply))
                <> "\r\n\r\n"
                <> reply
      )
      killThread
      ( \_ ->
          act port $
            timeout 5_000_000 (takeMVar bodyVar)
              >>= maybe (assertFailure "the server never received a request") pure
      )

{- | Read one HTTP request off the socket and return its body, as framed by
@Content-Length@ (a chunked request would be reported as a failure rather
than mis-parsed).
-}
readRequestBody :: Socket -> IO ByteString
readRequestBody conn = go BS.empty
  where
    go acc = case BS.breakSubstring "\r\n\r\n" acc of
      (headers, rest)
        | not (BS.null rest) -> do
            let body = BS.drop 4 rest
            case contentLength headers of
              Nothing -> fail ("request without a Content-Length: " <> show headers)
              Just n -> readUpTo n body
      _ -> recv conn 4096 >>= \chunk -> if BS.null chunk then pure acc else go (acc <> chunk)
    readUpTo n body
      | BS.length body >= n = pure (BS.take n body)
      | otherwise = recv conn 4096 >>= \chunk -> if BS.null chunk then pure body else readUpTo n (body <> chunk)
    contentLength headers =
      case [v | l <- BC.lines headers, Just v <- [BS.stripPrefix "content-length:" (BC.map toLowerAscii l)]] of
        (v : _) -> fst <$> BC.readInt (BC.strip v)
        [] -> Nothing
    toLowerAscii c = if isAsciiUpper c then toEnum (fromEnum c + 32) else c

{- | Run an action against a localhost HTTP server that answers its first
request with a streamed (SSE) OpenAI chat-completions reply: the given text
as the content, then a final chunk with @finish_reason: "length"@, then the
usage chunk and @[DONE]@ — what a server does when the output hits the token
cap. The client's @stream: true@ request is why the answer is SSE.
-}
withLengthStopServer :: T.Text -> (PortNumber -> IO a) -> IO a
withLengthStopServer content act =
  withListener $ \listener port ->
    bracket
      ( forkIO . void $ do
          (conn, _) <- accept listener
          bracket (pure conn) close $ \c -> do
            void (readRequestBody c)
            sendAll c $
              "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\nConnection: close\r\n\r\n"
                <> sseEvent (chunk (object ["role" .= ("assistant" :: T.Text), "content" .= content]) Null)
                <> sseEvent (chunk (object []) "length")
                <> sseEvent
                  ( object
                      [ "id" .= ("chatcmpl-test" :: T.Text)
                      , "object" .= ("chat.completion.chunk" :: T.Text)
                      , "created" .= (0 :: Int)
                      , "model" .= ("unused" :: T.Text)
                      , "choices" .= ([] :: [Value])
                      , "usage" .= object ["prompt_tokens" .= (10 :: Int), "completion_tokens" .= (4096 :: Int), "total_tokens" .= (4106 :: Int)]
                      ]
                  )
                <> "data: [DONE]\n\n"
      )
      killThread
      (\_ -> act port)
  where
    chunk delta finish =
      object
        [ "id" .= ("chatcmpl-test" :: T.Text)
        , "object" .= ("chat.completion.chunk" :: T.Text)
        , "created" .= (0 :: Int)
        , "model" .= ("unused" :: T.Text)
        , "choices" .= ([object ["index" .= (0 :: Int), "delta" .= delta, "finish_reason" .= (finish :: Value)]] :: [Value])
        ]
    sseEvent v = "data: " <> BL.toStrict (encode v) <> "\n\n"
