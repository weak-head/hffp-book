module ReaderT where

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  -- fmap :: forall a b. (a -> b) -> ReaderT r m a -> ReaderT r m b
  fmap f (ReaderT v) = ReaderT $ (fmap . fmap) f v

instance Applicative m => Applicative (ReaderT r m) where
  pure x = ReaderT $ pure $ pure x
  (ReaderT f) <*> (ReaderT v) = ReaderT $ (<*>) <$> f <*> v

instance Monad m => Monad (ReaderT r m) where
  return = pure
  -- (>>=) :: ReaderT r m a -> (a -> ReaderT r m b) -> ReaderT r m b
  (ReaderT v) >>= f = ReaderT $ 
    \r -> do
      v' <- v r
      runReaderT (f v') r
