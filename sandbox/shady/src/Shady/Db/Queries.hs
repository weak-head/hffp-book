{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Shady.Db.Queries
  ( initDb
  )
where

import Database.SQLite.Simple ( Query )
import Text.RawString.QQ

-- | Creates the initial database structure.
initDb :: Query
initDb = [r|
CREATE TABLE IF NOT EXISTS users
 ( id INTEGER PRIMARY KEY AUTOINCREMENT
 , login TEXT
 , reg_date TEXT
 );

CREATE TABLE IF NOT EXISTS messages
 ( id INTEGER PRIMARY KEY AUTOINCREMENT
 , sender INTEGER NOT NULL
 , receiver INTEGER NOT NULL
 , msg TEXT NOT NULL
 , FOREIGN KEY (sender) REFERENCES users (id) ON DELETE CASCADE
 , FOREIGN KEY (receiver) REFERENCES users (id) ON DELETE CASCADE
 );
|]

