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
import           Data.Maybe
import qualified Data.Text as TXT
import qualified Data.List as LST
import           Data.Text.Encoding ( decodeUtf8, encodeUtf8 )
import           Data.Time.Clock ( getCurrentTime )
import           Database.SQLite.Simple
import           Network.Socket
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NBS
import qualified Shady.Db as DB
import           Shady.Db.Exception
import qualified Shady.Db.Model as DBM
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
  dbCon <- eiGetDatabaseCon <$> ask

  let
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
loginHandler userName = do
  cs <- get
  if isJust . csUserLogin $ cs
    then
      respondLogError $ concat [ "You are alreadey logged in as <"
                               , fromJust . csUserLogin $ cs
                               , ">\n"
                               ]
    else do
      usr <- getUser userName
      case usr of
        -- here it would be good to have a check
        -- to figure out if the user is already used
        -- and logged-in from some other IP
        Just _ -> do
          put $ cs { csUserLogin = Just userName }
          respondClient "[Success]\n"

        Nothing -> do
          respondLogError $ concat [ "Failed to login. User <"
                                   , userName
                                   , "> does not exist.\n"
                                   ]

logoutHandler :: ClientHandler ()
logoutHandler = modify $ \cs -> cs { csIsAlive = False }

readHandler :: String -> ClientHandler ()
readHandler from = do
  whenAuthenticated getMsgs requestLogin

  where
    getMsgs to = do
      maybeFromUser <- getUser from
      maybeToUser   <- getUser to

      case maybeFromUser of
        Nothing       -> respondLogError "The specified user does not exist.\n"
        Just fromUser -> do
          dbCon <- eiGetDatabaseCon <$> ask

          let
            fromId        = DBM.userId fromUser
            toId          = DBM.userId $ fromJust maybeToUser
            getMessages = do
              liftIO $ withConnection dbCon (DB.getAllMessagesByUsers fromId toId)

          msgs <- getMessages `MC.catch` handleEx
          respondClient $ LST.intercalate "\n" (map (TXT.unpack . DBM.msg) msgs)

    -- we can use here something like (Either Error [Messages])
    handleEx :: ItemDoesNotExistException -> ClientHandler [DBM.Message]
    handleEx _ = respondLogError "We should never see this" >> return []

    requestLogin = do
      -- here we can request user to login and retry
      -- without leaving the handler
      respondLogError "Please login before sending a message.\n"

-- | Leave a message to the user.
sendHandler :: String           -- To user
            -> String           -- Message body
            -> ClientHandler ()
sendHandler to msg = do
  whenAuthenticated sendMsg requestLogin

  where
    sendMsg from = do
      maybeFromUser <- getUser from
      maybeToUser   <- getUser to

      case maybeToUser of
        Nothing     -> respondLogError "The specified user does not exist.\n"
        Just toUser -> do
          dbCon <- eiGetDatabaseCon <$> ask

          let
            fromId        = DBM.userId $ fromJust maybeFromUser
            toId          = DBM.userId toUser
            createMessage = do
              liftIO $ withConnection dbCon (DB.createMessage fromId toId msg)
              respondClient "[Success]\n"

          createMessage `MC.catch` handleEx

    handleEx :: ItemDoesNotExistException -> ClientHandler ()
    handleEx _ = respondLogError "We should never see this"

    requestLogin = do
      -- here we can request user to login and retry
      -- without leaving the handler
      respondLogError "Please login before sending a message.\n"

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

getUser :: String -> ClientHandler (Maybe DBM.User)
getUser userName = do
  dbCon <- eiGetDatabaseCon <$> ask
  liftIO $ withConnection dbCon (DB.getUserByLogin userName)

----------------------------------------

whenAuthenticated :: (String -> ClientHandler ()) -> ClientHandler () -> ClientHandler ()
whenAuthenticated hld err = do
  curUsr <- csUserLogin <$> get
  case curUsr of
    Just userName -> hld userName
    Nothing       -> err

-- | Writes log entry to console and to writer.
writeLog :: String -> ClientHandler ()
writeLog s = do
  now <- liftIO getCurrentTime
  let str = show now ++ " - " ++ s
  liftIO $ putStrLn str
  tell [str]
