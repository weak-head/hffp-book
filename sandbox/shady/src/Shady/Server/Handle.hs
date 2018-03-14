module Shady.Server.Handle
  (
    ClientHandler
  , ClientState(..)
  , EnvInfo(..)
  , DbConnInfo

  ------------------
  , handleClient

  ------------------
  , writeLog
  )
where

import           Control.Exception hiding ( handle )
import           Control.Monad ( void )
import qualified Control.Monad.Catch as MC
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Loops ( iterateWhile )
import           Control.Monad.Trans.Class ( lift )
import           Control.Monad.Trans.RWS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as DM
import           Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import           Data.Time.Clock ( getCurrentTime )
import           Database.SQLite.Simple
import           Network.Socket
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import qualified Shady.Db as DB
import           Shady.Db.Exception
import           Shady.Server.Commands
----------------------------------------

-- | The state of the connected client.
data ClientState =
  ClientState { csGetSock     :: Socket
              , csGetSockAddr :: SockAddr
              , csUserLogin   :: Maybe String
              , csIsAlive     :: Bool }
  deriving (Eq, Show)

-- | Represents the environment information:
--   - db connection
--   - arguments
--   - etc
data EnvInfo =
  EnvInfo { eiGetDatabaseCon :: DbConnInfo }
  deriving (Eq, Show)

type DbConnInfo      = String
type LogMessages     = [String] -- Data.ListD
type ClientHandler a = RWST EnvInfo LogMessages ClientState IO a

----------------------------------------

-- | The main client processing loop.
handleClient :: ClientHandler ()
handleClient = void $ iterateWhile csIsAlive $ do
  readCommand >>= handleCommand
  get

handleCommand :: Either String Command -> ClientHandler ()
handleCommand eth =
  case eth of
    Left err  -> parsingErrorHandler err
    Right cmd -> handle cmd

handle :: Command -> ClientHandler ()
handle (Register userName) = registerHandler userName
handle (Login userName)    = loginHandler userName
handle Logout              = logoutHandler
handle (Read from)         = readHandler from
handle (Send to message)   = sendHandler to message

--- Command Handlers -------------------

-- | Parse error handler.
parsingErrorHandler :: String -> ClientHandler ()
parsingErrorHandler _ = do
  let msg = "Failed to parse the command.\n"
  respondLogError msg

-- | Register a new user.
registerHandler :: String -> ClientHandler ()
registerHandler userName = do
  ei <- ask

  let
      dbCon      = eiGetDatabaseCon ei
      createUser = do
        liftIO $ withConnection dbCon (DB.createUser userName)
        respondClient $ concat [ "<"
                               , userName
                               , "> account has been created and is ready to use.\n"
                               ]

  createUser `MC.catch` handleEx

  where
    handleEx :: ItemAlreadyExistsException -> ClientHandler ()
    handleEx ex = do
      let
          msg = concat [ "Unable to create the user. The name <"
                       , itemName ex
                       , "> is already being used.\n"
                       ]
      respondLogError msg

-- | Login with existing user.
loginHandler :: String -> ClientHandler ()
loginHandler userName = undefined

logoutHandler :: ClientHandler ()
logoutHandler = modify $ \cs -> cs { csIsAlive = False }

readHandler :: String -> ClientHandler ()
readHandler from = undefined

sendHandler :: String -> String -> ClientHandler ()
sendHandler to msg = undefined

----------------------------------------

-- | Responds to client with the error message that is being logged.
respondLogError :: String -> ClientHandler ()
respondLogError msg = do
  cs <- get
  writeLog msg
  writeSocket (csGetSock cs) msg

-- | Respond to client with general message.
respondClient :: String -> ClientHandler ()
respondClient msg = do
  cs <- get
  writeSocket (csGetSock cs) msg

----------------------------------------

-- | Gets input from client and parses into command.
readCommand :: ClientHandler (Either String Command)
readCommand = do
  cmdStr <- get >>= readSocket . csGetSock
  lift . return $ parseCommand cmdStr

-- | Read input from socket.
readSocket :: Socket -> ClientHandler BS.ByteString
readSocket sock = liftIO $ NBS.recv sock 1024

-- | Writes string to socket.
writeSocket :: Socket -> String -> ClientHandler ()
writeSocket sock msg = liftIO $ NBS.sendAll sock (BSC.pack msg)

----------------------------------------

-- | Writes log entry to console and to writer.
writeLog :: String -> ClientHandler ()
writeLog s = do
  now <- liftIO getCurrentTime
  let str = show now ++ " - " ++ s
  liftIO $ putStrLn str
  tell [str]
