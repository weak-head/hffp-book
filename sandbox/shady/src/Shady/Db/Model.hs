module Shady.Db.Model
  ( User (..)
  , Message (..)
  )
where

import Database.SQLite.Simple
import Data.Text

data User =
  User { userId :: Integer
       , login :: Text
       , regDate :: Text
       } deriving (Eq, Show)

data Message =
  Message { messageId :: Integer
          , sender :: Text
          , receiver :: Text
          , msg :: Text
          } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ lg reg) =
    toRow (id_, lg, reg)

instance FromRow Message where
  fromRow = Message <$> field
                    <*> field
                    <*> field
                    <*> field

instance ToRow Message where
  toRow (Message id_ s r m) =
    toRow (id_, s, r, m)
