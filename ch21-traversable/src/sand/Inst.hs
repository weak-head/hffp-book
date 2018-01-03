{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

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

--- Foldable ---------------------------------------------------------

instance {-# OVERLAPPING #-} Monoid_ (a -> a) where
  mempty_ = id
  mappend_ = (.)

class Foldable_ t where
  {-# MINIMAL (foldr_ | foldMap_) #-}

  foldMap_ :: Monoid_ m => (a -> m) -> t a -> m
  foldMap_ f = foldr_ (mappend_ . f) mempty_

  foldr_ :: (a -> b -> b) -> b -> t a -> b
  foldr_ f d xs = foldMap_ (\a -> f a) xs d

  fold_ :: Monoid_ m => t m -> m

  foldr_' :: (a -> b -> b) -> b -> t a -> b

  foldl_ :: (b -> a -> b) -> b -> t a -> b

  foldl_' :: (b -> a -> b) -> b -> t a -> b

  foldr1 :: (a -> a -> a) -> t a -> a

  foldl1 :: (a -> a -> a) -> t a -> a

  toList :: t a -> [a]

  null :: t a -> Bool

  length :: t a -> Int

  elem :: Eq a => e -> t a -> Bool

  maximum :: forall a. Ord a => t a -> a

  minimum :: forall a. Ord a => t a -> a

  sum :: Num a => t a -> a

  product :: Num a => t a -> a
