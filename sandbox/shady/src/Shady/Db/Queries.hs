{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Shady.Db.Queries
  ( createUsers
  , createMessages
  ----------------
  , insertUser
  , getUserByLogin
  )
where

import Database.SQLite.Simple ( Query )
import Text.RawString.QQ

-- | Creates user table.
createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
 ( id INTEGER PRIMARY KEY AUTOINCREMENT
 , login TEXT
 , reg_date TEXT
 );
|]

-- | Creates messages table.
createMessages :: Query
createMessages = [r|
CREATE TABLE IF NOT EXISTS messages
 ( id INTEGER PRIMARY KEY AUTOINCREMENT
 , sender INTEGER NOT NULL
 , receiver INTEGER NOT NULL
 , msg TEXT NOT NULL
 , FOREIGN KEY (sender) REFERENCES users (id) ON DELETE CASCADE
 , FOREIGN KEY (receiver) REFERENCES users (id) ON DELETE CASCADE
 );
|]

-- | Insert new user.
insertUser :: Query
insertUser = [r|
INSERT INTO users
VALUES (?, ?, ?)
|]

-- | Get user by login.
getUserByLogin :: Query
getUserByLogin = [r|
SELECT *
FROM users AS u
WHERE u.login = ?
|]
