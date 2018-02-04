module MaybeT where

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

{-

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

-}
-- Nothing special here
instance Functor m => Functor (MaybeT m) where
  fmap f (MaybeT m) = MaybeT $ (fmap . fmap) f m

{-

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ pure $ pure x
  (Compose f) <*> (Compose v) = Compose $ fmap (<*>) f <*> v
-}
-- Same as compose
instance Applicative m => Applicative (MaybeT m) where
  pure = MaybeT . pure . Just
  (MaybeT f) <*> (MaybeT v) = MaybeT $ fmap (<*>) f <*> v



instance Monad m => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f =
    MaybeT $ do
      v <- ma
      case v of
        Nothing -> return Nothing
        Just a  -> runMaybeT $ f a
