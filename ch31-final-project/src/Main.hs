{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception
import           Control.Monad (forever, void)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.List (intersperse)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Typeable
import qualified Database.SQLite.Simple as SQLite
import           Database.SQLite.Simple hiding (close)
import           Database.SQLite.Simple.Types
import           Network.Socket hiding (close, recv)
import qualified Network.Socket as NS
import           Network.Socket.ByteString (recv, sendAll)
import           Text.RawString.QQ

----------------------------------------

data User =
  User { userId :: Integer
       , username :: Text
       , shell :: Text
       , homeDirectory :: Text
       , realName :: Text
       , phone :: Text
       } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

----------------------------------------

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  ( id INTEGER PRIMARY KEY AUTOINCREMENT
  , username TEXT UNIQUE
  , shell TEXT
  , homeDirectory TEXT
  , realName TEXT
  , phone TEXT )
|]

insertUser :: Query
insertUser = [r|
INSERT INTO users
VALUES (?, ?, ?, ?, ?, ?)
|]

allUsers :: Query
allUsers = [r|
SELECT * FROM users
|]

getUserQuery :: Query
getUserQuery = [r|
SELECT *
FROM users
WHERE username = ?
|]

----------------------------------------

data DatabaseException =
  forall e . Exception e => DatabaseException e

instance Show DatabaseException where
  show (DatabaseException e) = show e

instance Exception DatabaseException

dbExceptionToException :: Exception e => e -> SomeException
dbExceptionToException = toException . DatabaseException

dbExceptionFromException :: Exception e => SomeException -> Maybe e
dbExceptionFromException x = do
  DatabaseException e <- fromException x
  cast e

----

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData where
  toException = dbExceptionToException
  fromException = dbExceptionFromException

----------------------------------------

type UserRow =
  (Null, Text, Text, Text, Text, Text)

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do
  results <- query conn getUserQuery (Only username)
  case results of
    []     -> return Nothing
    [user] -> return $ Just user
    _      -> throwIO DuplicateData

createDatabase :: IO ()
createDatabase = do
  conn <- open "f.db"
  execute_ conn createUsers
  execute conn insertUser userRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where userRow :: UserRow
        userRow = ( Null
                  , "suser"
                  , "/bin/bash"
                  , "/home/suser"
                  , "S User"
                  , "123-456-7890")

----------------------------------------

returnUsers :: Connection
            -> Socket
            -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSep = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSep)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) =
  BS.concat [ "Login: ",     e username, "\t\t\t\t"
            , "Name: ",      e realName, "\n"
            , "Directory: ", e homeDir,  "\t\t\t"
            , "Shell: ",     e shell,    "\n" ]
  where e = encodeUtf8

returnUser :: Connection
           -> Socket
           -> Text
           -> IO ()
returnUser con sock uname = do
  mUser <- getUser con (T.strip uname)
  case mUser of
    Nothing   -> void $ putStrLn $ "Coudn't find matching user for username: " ++ show uname
    Just user -> sendAll sock (formatUser user)

handleQuery :: Connection
            -> Socket
            -> IO ()
handleQuery con soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers con soc
    name   -> returnUser con soc (decodeUtf8 name)

handleQueries :: Connection
              -> Socket
              -> IO ()
handleQueries con sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery con soc
  NS.close soc

main :: IO ()
main = withSocketsDo $ do
  addrinfos <- getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
                           Nothing
                           (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr)
                 Stream defaultProtocol
  NS.bind sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "f.db"
  handleQueries conn sock
  SQLite.close conn
  NS.close sock
