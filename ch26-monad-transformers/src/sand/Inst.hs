module Inst where

----------------------------------------------------------------------

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT f) <*> (MaybeT m) =
    MaybeT $ (<*>) <$> f <*> m

instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT m) >>= f =
    MaybeT $ m >>= maybe (return Nothing) (runMaybeT . f)

----------------------------------------------------------------------

newtype ExceptT e m a =
  ExceptT { runExceptT :: m (Either e a) }

instance Functor m => Functor (ExceptT e m) where
  fmap f (ExceptT m) = ExceptT $ (fmap . fmap) f m

instance Applicative m => Applicative (ExceptT e m) where
  pure = ExceptT . pure . pure
  (ExceptT f) <*> (ExceptT m) =
    ExceptT $ (<*>) <$> f <*> m

instance Monad m => Monad (ExceptT e m) where
  return = pure
  (ExceptT m) >>= f =
    ExceptT $ m >>= either (return . Left) (runExceptT . f)

----------------------------------------------------------------------

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT m) = ReaderT $ (fmap . fmap) f m

instance Applicative m => Applicative (ReaderT r m) where
  pure = ReaderT . pure . pure
  (ReaderT f) <*> (ReaderT m) =
    ReaderT $ (<*>) <$> f <*> m

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT m) >>= f =
    ReaderT $ \r ->
      m r >>= flip runReaderT r . f

----------------------------------------------------------------------

newtype StateT s m a =
  StateT { runStateT :: s -> m (a, s) }
