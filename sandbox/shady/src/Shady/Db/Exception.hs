{-# LANGUAGE ExistentialQuantification #-}

module Shady.Db.Exception where

import Data.Typeable
import Control.Exception

----------------------------------------

-- | Base class of all database related exceptions.
data DbException = forall e . Exception e => DbException e

instance Show DbException where
  show (DbException e) = show e

instance Exception DbException

databaseExceptionToException :: Exception e => e -> SomeException
databaseExceptionToException = toException . DbException

databaseExceptionFromException :: Exception e => SomeException -> Maybe e
databaseExceptionFromException x = do
  DbException e <- fromException x
  cast e

----------------------------------------

-- | All data related database exceptions.
data DbDataException = forall e . Exception e => DbDataException e
  
instance Show DbDataException where
  show (DbDataException e) = show e

instance Exception DbDataException where
  toException = databaseExceptionToException
  fromException = databaseExceptionFromException

dbDataExceptionToException :: Exception e => e -> SomeException
dbDataExceptionToException = toException . DbDataException

dbDataExceptionFromException :: Exception e => SomeException -> Maybe e
dbDataExceptionFromException x = do
  DbDataException e <- fromException x
  cast e

----------------------------------------

data ItemAlreadyExistsException =
  ItemAlreadyExistsException { itemName :: String }
  deriving (Show)

instance Exception ItemAlreadyExistsException where
  toException = dbDataExceptionToException
  fromException = dbDataExceptionFromException

data ItemDoesNotExistException =
  ItemDoesNotExistException { item :: String }
  deriving (Show)

instance Exception ItemDoesNotExistException where
  toException = dbDataExceptionToException
  fromException = dbDataExceptionFromException
