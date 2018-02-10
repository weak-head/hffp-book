{-# LANGUAGE OverloadedStrings #-}

module LiftIO where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class -- (lift)
import Data.Monoid (mconcat)
import EitherT
import MaybeT
import MonadTrans
import ReaderT
import StateT
import Web.Scotty

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
    liftIO $ putStrLn "hello"
    html $ mconcat [ "<h1>Scotty, "
                   , beam
                   , " me up!</h1>" ]

----------------------------------------------------------------------

--newtype IdentityT m a =
--  IdentityT { runIdentityT :: m a }

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT m) = IdentityT $ fmap f m

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  (IdentityT f) <*> (IdentityT v) =
    IdentityT $ f <*> v

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT m) >>= f =
    IdentityT $ m >>= runIdentityT . f

instance MonadIO m => MonadIO (IdentityT m) where
  liftIO = IdentityT . liftIO

----------------------------------------------------------------------

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = lift . liftIO

----------------------------------------------------------------------

instance MonadIO m => MonadIO (MaybeT m) where
  liftIO = lift . liftIO

----------------------------------------------------------------------

instance MonadIO m => MonadIO (ReaderT r m) where
  liftIO = lift . liftIO

----------------------------------------------------------------------

instance MonadIO m => MonadIO (StateT s m) where
  liftIO = lift . liftIO
