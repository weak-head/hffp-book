module Inst where


class Monoid_ a where
  mempty_ :: a

  mappend_ :: a -> a -> a

  mconcat_ :: [a] -> a
  mconcat_ = foldr mappend_ mempty_


class Functor_ f where
  fmap_ :: (a -> b) -> f a -> f b

  (<$!) :: a -> f b -> f a
  (<$!) = fmap_ . const


class Functor_ f => Applicative_ f where
  {-# MINIMAL pure_, (app_ | liftA2) #-}
  pure_ :: a -> f a

  app_ :: f (a -> b) -> f a -> f b
  app_ = liftA2 id

  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  liftA2 f a b = (f `fmap_` a) `app_` b

  (*>!) :: f a -> f b -> f b
  (*>!) a b = (id <$! a) `app_` b

  (<*!) :: f a -> f b -> f a
  (<*!) = liftA2 const

