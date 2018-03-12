module Main where

import Database.SQLite.Simple
import Shady.Server
import Shady.Db

main :: IO ()
main = do
  let dbCon = "shady.db"
      port  = 4343
  withConnection dbCon initDb
  startServer dbCon port
