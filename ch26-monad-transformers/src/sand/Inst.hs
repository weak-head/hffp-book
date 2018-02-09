module Inst where

----------------------------------------------------------------------

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . pure
  (MaybeT f) <*> (MaybeT m) =
    MaybeT $ fmap (<*>) f <*> m

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
    ExceptT $ fmap (<*>) f <*> m

instance Monad m => Monad (ExceptT e m) where
  return = pure
  (ExceptT m) >>= f =
    ExceptT $ m >>= either (return . Left) (runExceptT . f)
    
