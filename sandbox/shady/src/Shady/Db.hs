module Shady.Db where

import Data.Maybe ( isJust )
import Control.Exception
import Data.Time.Clock
import Shady.Db.Model
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import qualified Shady.Db.Queries as Q

-- | Creates and initialize database, if required.
initDb :: Connection -> IO ()
initDb con =
  withExclusiveTransaction con $ do
    execute_ con Q.createUsers
    execute_ con Q.createMessages

-- | Creates a new user with the specified user name.
createUser :: String -> Connection -> IO ()
createUser userName con = do
  exists <- userExists userName con
  if exists
    then putStrLn "User exists" -- throw
    else do
      now <- getCurrentTime
      execute con Q.insertUser (Null, userName, now)

-- | Returns true if the user with the name exists.
userExists :: String -> Connection -> IO Bool
userExists userName con =
  isJust <$> getUserByLogin userName con

-- | Get user by login.
getUserByLogin :: String -> Connection -> IO (Maybe User)
getUserByLogin userName con = do
  res <- query con Q.getUserByLogin (Only userName)
  case res of
    []     -> return Nothing
    [user] -> return $ Just user
--    _      -> throw
