{-# LANGUAGE OverloadedStrings #-}

module Shady.Server
  ( startServer
  )
where

import           Control.Concurrent
import           Control.Monad
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS

type ConnectionInfo = String
type Port           = Int

-- | Start shady server.
startServer :: ConnectionInfo -> Port -> IO ()
startServer con port = do
  addr <- getAddress port
  sock <- socket (addrFamily addr) Stream defaultProtocol
  bind sock (addrAddress addr)
  listen sock 5
  handleClients con sock
  close sock

-- | Handle connections from clients.
handleClients :: ConnectionInfo -> Socket -> IO ()
handleClients con sock = forever $ do
  (soc, _) <- accept sock
  forkIO $ void $ handleClient con soc

-- | Handle single client.
handleClient :: ConnectionInfo -> Socket -> IO ()
handleClient con sock = do
  cmd <- NBS.recv sock 1024
  print $ decodeUtf8 cmd
  close sock

-- | Gets address to bind the socket.
getAddress :: Port -> IO AddrInfo
getAddress port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                       Nothing
                       (Just $ show port)
