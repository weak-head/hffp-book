{-# LANGUAGE OverloadedStrings #-}

module Shady.Server
  ( startServer
  )
where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Concurrent
import           Control.Monad
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS

----------------------------------------

-- | Contains the information about the socket and
-- database connection.
data ConnectionInfo =
  ConnectionInfo { dbInfo   :: DbConnInfo
                 , sockInfo :: Socket }
  deriving (Eq, Show)

type Handler    = ReaderT ConnectionInfo IO ()
type DbConnInfo = String
type Port       = Int

----------------------------------------

-- | Start shady server.
startServer :: DbConnInfo -> Port -> IO ()
startServer con port = do
  addr <- getAddress port
  sock <- socket (addrFamily addr) Stream defaultProtocol
  bind sock (addrAddress addr)
  listen sock 5
  runReaderT handleClients (ConnectionInfo con sock)
  close sock

-- | Handle connections from clients.
handleClients :: Handler
handleClients = ReaderT $ \ci -> forever $ do
  (soc, _) <- accept (sockInfo ci)
  forkIO $ runReaderT handleClient (ci { sockInfo = soc }) >> close soc

-- | Handle single client.
handleClient :: Handler
handleClient = ReaderT $ \ci -> do
  cmd <- NBS.recv (sockInfo ci) 1024
  print $ decodeUtf8 cmd

-- | Gets address to bind the socket.
getAddress :: Port -> IO AddrInfo
getAddress port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                       Nothing
                       (Just $ show port)
