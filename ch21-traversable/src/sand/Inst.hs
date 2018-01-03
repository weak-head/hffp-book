module Inst where

--- Monoid -----------------------------------------------------------

class Monoid_ a where
  mempty_ :: a

  mappend_ :: a -> a -> a

  mconcat_ :: [a] -> a
  mconcat_ = foldr mappend_ mempty_

--- Functor ----------------------------------------------------------

class Functor_ f where
  fmap_ :: (a -> b) -> f a -> f b

  (<$!) :: a -> f b -> f a
  (<$!) = fmap_ . const

infixl 4 <$>!
(<$>!) :: Functor_ f => (a -> b) -> f a -> f b
(<$>!) =  fmap_

--- Applicative ------------------------------------------------------

class Functor_ f => Applicative_ f where
  {-# MINIMAL pure_, ((<*>!) | liftA2_) #-}
  pure_ :: a -> f a

  (<*>!) :: f (a -> b) -> f a -> f b
  (<*>!) = liftA2_ id

  liftA2_ :: (a -> b -> c) -> f a -> f b -> f c
  liftA2_ f a b = (f `fmap_` a) <*>! b

  (*>!) :: f a -> f b -> f b
  (*>!) a b = (id <$! a) <*>! b

  (<*!) :: f a -> f b -> f a
  (<*!) = liftA2_ const

--- Monad ------------------------------------------------------------

class Applicative_ m => Monad_ m where
  return_ :: a -> m a
  return_ = pure_

  (>>=!) :: m a -> (a -> m b) -> m b

  (>>!) :: m a -> m b -> m b
  (>>!) a b = a >>=! const b

infixr 1 =<<!
(=<<!) :: Monad_ m => (a -> m b) -> m a -> m b
(=<<!) f m = m >>=! f

infixr 1 >=>!
(>=>!) :: Monad_ m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>!) f g a = f a >>=! g
