{-# LANGUAGE OverloadedStrings #-}

module Shady.Server
  ( startServer
  )
where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.Monad.Trans.Reader as TR
import           Control.Monad.Trans.RWS
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.Socket
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS

----------------------------------------

-- | Contains the information about the socket and
-- database connection.
data ConnectionInfo =
  ConnectionInfo { getDbInfo :: DbConnInfo
                 , getSocket :: Socket }
  deriving (Eq, Show)

-- | Represents the environment information:
--   - db connection
--   - arguments
--   - etc
data EnvInfo =
  EnvInfo { getDatabaseCon :: DbConnInfo }
  deriving (Eq, Show)

-- | The state of the connected client.
data ClientState =
  ClientState { getSock :: Socket
              , getSockAddr :: SockAddr }
  deriving (Eq, Show)

type Handler = TR.ReaderT ConnectionInfo IO ()
type ClientHandler a = RWST EnvInfo LogMessages ClientState IO a

type LogMessages = [String] -- ListD
type DbConnInfo  = String
type Port        = Int

----------------------------------------

-- | Start shady server.
startServer :: DbConnInfo -> Port -> IO ()
startServer con port = do
  addr <- getAddress port
  sock <- socket (addrFamily addr) Stream defaultProtocol
  bind sock (addrAddress addr)
  NS.listen sock 5
  TR.runReaderT handleClients (ConnectionInfo con sock)
  close sock

-- | Handle connections from clients.
handleClients :: Handler
handleClients = forever $ do
  ci <- TR.ask
  (soc, socadr) <- liftIO $ accept (getSocket ci)  
  liftIO $ forkIO $ void $ evalRWST (handleClient >> closeConn)
                                    (mkEnv ci)
                                    (mkState soc socadr)
  where
    mkEnv (ConnectionInfo db _) = EnvInfo db
    mkState s a = ClientState s a
    closeConn = get >>= liftIO . close . getSock

handleClient :: ClientHandler ()
handleClient = do
  cs <- get
  cmd <- liftIO $ NBS.recv (getSock cs) 1024
  liftIO $ print $ decodeUtf8 cmd

-- | Gets address to bind the socket.
getAddress :: Port -> IO AddrInfo
getAddress port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                       Nothing
                       (Just $ show port)
