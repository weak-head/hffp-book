module ExceptT where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- newtype ExceptT e m a =
--   ExceptT { runExceptT :: m (Either e a) }

-- newtype MaybeT m a =
--   MaybeT { runMaybeT :: m (Maybe a) }

-- newtype ReaderT r m a =
--   ReaderT { runReaderT :: r -> m a }

-- One big monad
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

-- -- --

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

evaluate :: IO (Either String (Maybe Int))
evaluate = readerUnwrap ()
-- Right (Just 1)

showEvaluate :: IO ()
showEvaluate = evaluate >>= print
-- Right (Just 1)
