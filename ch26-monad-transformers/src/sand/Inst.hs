module Inst where

----------------------------------------------------------------------

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }


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
    
