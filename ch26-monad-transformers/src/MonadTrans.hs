module MonadTrans where

import Control.Monad (liftM)
import EitherT
import ReaderT
import StateT
import MaybeT

{-

fmap :: Functor f =>      (a -> b) -> f a -> f b

liftA :: Applicative f => (a -> b) -> f a -> f b

liftM :: Monad m =>       (a -> b) -> m a -> m b

-}

class MonadTrans t where
  lift :: (Monad m) => m a -> t m a

----------------------------------------------------------------------

newtype IdentityT m a = IdentityT { runIdentityT :: m a }

instance MonadTrans IdentityT where
  lift = IdentityT

----------------------------------------------------------------------

instance MonadTrans MaybeT where
  lift = MaybeT . liftM Just

----------------------------------------------------------------------

instance MonadTrans (ReaderT r) where
  lift = ReaderT . const

----------------------------------------------------------------------

instance MonadTrans (EitherT e) where
  lift = EitherT . liftM Right

----------------------------------------------------------------------

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> liftM (flip (,) s) m
