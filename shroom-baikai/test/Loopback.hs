{- | A loopback HTTP listener for tests that run a backend against a server of
their own, with no network and no model.

It binds port 0 on @127.0.0.1@ (the OS picks a free port, so parallel runs
cannot collide) and is shared by the wire contract suite ("WireContract") and
the offline suite (@test\/Offline\/Main.hs@), which each used to carry their
own copy of it.
-}
module Loopback (
  Captured (..),
  withListener,
  withCannedServer,
  readRequest,
) where

-- base
import Control.Concurrent (forkIO, killThread)
import Control.Exception (SomeException, bracket, handle)
import Control.Monad (forever)
import Data.Char (isAsciiUpper)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef)
import Data.Maybe (fromMaybe)

-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC

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

-- | One recorded request.
data Captured = Captured
  { requestLine :: ByteString
  , headers :: [(ByteString, ByteString)]
  -- ^ Header names lower-cased.
  , body :: ByteString
  }

{- | Run an action against a localhost TCP listener on a free port. Binds port
0, so the OS picks a free one and parallel runs cannot collide; localhost
only, no external service. The action gets the listening socket, to accept
connections on itself, and the port.
-}
withListener :: (Socket -> PortNumber -> IO a) -> IO a
withListener act =
  bracket (socket AF_INET Stream defaultProtocol) close $ \listener -> do
    bind listener (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
    listen listener 4
    port <-
      getSocketName listener >>= \case
        SockAddrInet p _ -> pure p
        other -> fail ("unexpected listener address: " <> show other)
    act listener port

{- | Run an action against a loopback HTTP listener that serves one canned
reply per request in order, answers 500 once they run out, and records every
request. The action gets the port and a read of what has been recorded so
far; the listener records a request before it replies, so a read after the
client call returned sees that request.

Each reply is a complete HTTP response and closes its connection, so every
request arrives on a fresh connection.
-}
withCannedServer :: [ByteString] -> (PortNumber -> IO [Captured] -> IO a) -> IO a
withCannedServer replies act =
  withListener $ \listener port -> do
    recorded <- newIORef []
    remaining <- newIORef replies
    let serve = forever $ do
          (conn, _) <- accept listener
          handle (\(_ :: SomeException) -> pure ()) . bracket (pure conn) close $ \c -> do
            req <- readRequest c
            modifyIORef' recorded (<> [req])
            next <- atomicModifyIORef' remaining (\case r : rest -> (rest, Just r); [] -> ([], Nothing))
            sendAll c (fromMaybe internalServerError next)
    bracket (forkIO serve) killThread (\_ -> act port (readIORef recorded))
  where
    internalServerError :: ByteString
    internalServerError = "HTTP/1.1 500 Internal Server Error\r\nConnection: close\r\nContent-Length: 0\r\n\r\n"

{- | Read one HTTP request off the socket: request line, headers (names
lower-cased) and a body as framed by @Content-Length@ (a request without one
is a failure rather than a mis-parse).
-}
readRequest :: Socket -> IO Captured
readRequest conn = go BS.empty
  where
    go acc = case BS.breakSubstring "\r\n\r\n" acc of
      (headBlock, rest)
        | not (BS.null rest) -> do
            let (reqLine, hdrs) = case BC.lines (BC.filter (/= '\r') headBlock) of
                  l : ls -> (l, [(BC.map lower k, BC.strip (BS.drop 1 v)) | line <- ls, let (k, v) = BC.break (== ':') line])
                  [] -> (BS.empty, [])
            case lookup "content-length" hdrs >>= fmap fst . BC.readInt of
              Nothing -> fail ("request without a Content-Length: " <> show headBlock)
              Just n -> do
                bodyBytes <- readUpTo n (BS.drop 4 rest)
                pure Captured {requestLine = reqLine, headers = hdrs, body = bodyBytes}
      _ -> more acc >>= maybe (fail ("connection closed mid-request: " <> show acc)) go
    readUpTo n acc
      | BS.length acc >= n = pure (BS.take n acc)
      | otherwise = more acc >>= maybe (fail "connection closed mid-body") (readUpTo n)
    more acc = recv conn 4096 >>= \chunk -> pure (if BS.null chunk then Nothing else Just (acc <> chunk))
    lower c = if isAsciiUpper c then toEnum (fromEnum c + 32) else c
