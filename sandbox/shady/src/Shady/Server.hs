{-# LANGUAGE OverloadedStrings #-}

module Shady.Server
  ( startServer
  )
where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Trans.RWS
import qualified Control.Monad.Trans.Reader as TR
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time.Clock ( getCurrentTime )
import           Network.Socket
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import           Shady.Server.Handle

----------------------------------------

-- | Contains the information about the socket and
-- database connection.
data ConnectionInfo =
  ConnectionInfo { getDbInfo :: DbConnInfo
                 , getSocket :: Socket }
  deriving (Eq, Show)

type Handler = TR.ReaderT ConnectionInfo IO ()

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
  liftIO $ forkIO $ void $ evalRWST processSingleClient
                                    (mkEnv ci)
                                    (mkState soc socadr)
  where
    mkEnv (ConnectionInfo db _) =
      EnvInfo { getDatabaseCon = db }
    mkState s a =
      ClientState { getSock = s
                  , getSockAddr = a
                  , isAuth = False
                  , isAlive = True
                  }

-- | Process the newly connected client.
processSingleClient :: ClientHandler ()
processSingleClient = do
  cs <- get
  let sadr = show (getSockAddr cs)
  writeLog $ "Connected: " ++ sadr
  handleClient
  liftIO . close . getSock $ cs
  writeLog $ "Disconnected: " ++ sadr

-- | Gets address to bind the socket.
getAddress :: Port -> IO AddrInfo
getAddress port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                       Nothing
                       (Just $ show port)
