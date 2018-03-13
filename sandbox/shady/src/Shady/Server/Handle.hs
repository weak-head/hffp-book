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
  ClientState { getSock :: Socket
              , getSockAddr :: SockAddr
              , isAuth :: Bool
              , isAlive :: Bool }
  deriving (Eq, Show)

-- | Represents the environment information:
--   - db connection
--   - arguments
--   - etc
data EnvInfo =
  EnvInfo { getDatabaseCon :: DbConnInfo }
  deriving (Eq, Show)

type DbConnInfo      = String
type LogMessages     = [String] -- Data.ListD
type ClientHandler a = RWST EnvInfo LogMessages ClientState IO a

----------------------------------------

-- | The main client processing loop.
handleClient :: ClientHandler ()
handleClient = void $ iterateWhile isAlive $ do
  readCommand >>= handleCommand
  get

----------------------------------------

-- | Gets input from client and parses into command.
readCommand :: ClientHandler (Either String Command)
readCommand = do
  cmdStr <- get >>= readSocket . getSock
  lift . return $ parseCommand cmdStr

-- | Read input from socket.
readSocket :: Socket -> ClientHandler BS.ByteString
readSocket sock = liftIO $ NBS.recv sock 1024

-- | Writes string to socket.
writeSocket :: Socket -> String -> ClientHandler ()
writeSocket sock msg = liftIO $ NBS.sendAll sock (BSC.pack msg)

----------------------------------------

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

-- | Parse error handler.
parsingErrorHandler :: String -> ClientHandler ()
parsingErrorHandler errMsg = do
  let msg = "Failed to parse the command.\n"
  cs <- get
  writeLog msg
  writeSocket (getSock cs) msg

-- | Register a new user.
registerHandler :: String -> ClientHandler ()
registerHandler userName = do
  ei <- ask
  let dbCon = getDatabaseCon ei
  liftIO $ withConnection dbCon (DB.createUser userName) `catch` handleEx 
  where
    handleEx :: ItemAlreadyExistsException -> IO ()
    handleEx = print

loginHandler :: String -> ClientHandler ()
loginHandler = undefined

logoutHandler :: ClientHandler ()
logoutHandler = modify $ \cs -> cs { isAlive = False }

readHandler :: String -> ClientHandler ()
readHandler from = undefined

sendHandler :: String -> String -> ClientHandler ()
sendHandler to msg = undefined

----------------------------------------

-- | Writes log entry to console and to writer.
writeLog :: String -> ClientHandler ()
writeLog s = do
  now <- liftIO getCurrentTime
  let str = show now ++ " - " ++ s
  liftIO $ putStrLn str
  tell [str]
