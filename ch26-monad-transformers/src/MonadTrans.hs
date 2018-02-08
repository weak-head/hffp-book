module MonadTrans where

{-

fmap :: Functor f =>      (a -> b) -> f a -> f b

liftA :: Applicative f => (a -> b) -> f a -> f b

liftM :: Monad m =>       (a -> b) -> m a -> m b

-}

{-

class MonadTrans t where
  lift : : (Monad m) => m a -> t m a

-}
