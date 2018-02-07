module StateT where

import Control.Arrow

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }

instance Functor m => Functor (StateT s m) where
  -- (fmap) :: (a -> b) -> StateT s m a -> StateT s m b
  fmap f (StateT v) = StateT $ (fmap . fmap) (first f) v

instance Monad m => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)
  -- (<*>) :: StateT s m (a -> b) -> StateT s m a -> StateT s m b
  (StateT f) <*> v = StateT $
    \s -> do
      (g, s') <- f s
      runStateT (fmap g v) s'
