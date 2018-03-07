module Shady.Db where

import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import qualified Shady.Db.Queries as Q

someFunc :: String
someFunc = "running"

-- | Creates and initialize database, if required.
initDb :: IO ()
initDb = do
  conn <- open "shady.db"
  execute_ conn Q.initDb
