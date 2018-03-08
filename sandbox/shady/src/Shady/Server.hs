{-# LANGUAGE OverloadedStrings #-}

module Shady.Server
  ( startServer
  )
where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
--import           Control.Monad.Trans.RWS
import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.Writer as TW
import           Control.Monad.Trans.Error
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.Socket
import qualified Network.Socket.ByteString as NBS

----------------------------------------

-- | Contains the information about the socket and
-- database connection.
data ConnectionInfo =
  ConnectionInfo { getDbInfo :: DbConnInfo
                 , getSocket :: Socket }
  deriving (Eq, Show)

data EnvInfo =
  EnvInfo
  deriving (Eq, Show)

data ClientState =
  ClientState
  deriving (Eq, Show)

data ActionResult =
  ActionResult
  deriving (Eq, Show)

type Handler = ReaderT ConnectionInfo IO ()

type NetworkError = String
type LogMessages = [String] -- ListD
type ClientHandler a = ReaderT ConnectionInfo (TW.WriterT LogMessages (StateT ClientState IO)) a

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
handleClients = forever $ do
  ci <- ask
  (soc, _) <- liftIO $ accept (getSocket ci)
  liftIO $ forkIO $ runReaderT handleClient (ci { getSocket = soc }) >> close soc

-- | Handle single client.
--handleClient :: CmdLet
handleClient = ReaderT $ \ci -> do
  cmd <- NBS.recv (getSocket ci) 1024
  print $ decodeUtf8 cmd

-- | Gets address to bind the socket.
getAddress :: Port -> IO AddrInfo
getAddress port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                       Nothing
                       (Just $ show port)
