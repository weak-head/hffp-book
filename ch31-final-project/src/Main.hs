{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Exception
import           Control.Monad (forever)
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

main :: IO ()
main = do
  putStrLn "hello world"
