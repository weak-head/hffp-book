{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}

module Inst where

import Data.Coerce

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

instance Monoid_ (a -> a) where
  mempty_ = id
  mappend_ = (.)

newtype Sum_ a = Sum_ { unSum_ :: a } deriving (Show, Eq, Ord)
instance Num a => Monoid_ (Sum_ a) where
  mempty_ = Sum_ 0
  mappend_ = coerce ((+) :: a -> a -> a)

newtype Prod_ a = Prod_ { unProd_ :: a } deriving (Show, Eq, Ord)
instance Num a => Monoid_ (Prod_ a) where
  mempty_ = Prod_ 1
  mappend_ = coerce ((*) :: a -> a -> a)

class Foldable_ t where
  {-# MINIMAL (foldr_ | foldMap_) #-}

  foldMap_ :: Monoid_ m => (a -> m) -> t a -> m
  foldMap_ f = foldr_ (mappend_ . f) mempty_

  foldr_ :: (a -> b -> b) -> b -> t a -> b
  foldr_ f d xs = foldMap_ f xs d

  fold_ :: Monoid_ m => t m -> m
  fold_ = foldMap_ id

  foldr_' :: (a -> b -> b) -> b -> t a -> b
  foldr_' f = foldr_ (\v a -> a `seq` f v a)
  -- foldr_ (\v a -> "(" ++ show v ++ " f " ++ a ++ ")") "" [1,2,3]

  foldl_ :: (b -> a -> b) -> b -> t a -> b
  foldl_ f d xs = foldr_ (\a g b -> g (f b a)) id xs d
  -- foldl_ (\a v -> "(" ++ a ++ " f " ++ show v ++ ")") "" [1,2,3]

  foldl_' :: (b -> a -> b) -> b -> t a -> b
  foldl_' f = foldl_' (\a v -> a `seq` f a v)

  foldr1_ :: (a -> a -> a) -> t a -> a

  foldl1_ :: (a -> a -> a) -> t a -> a

  toList_ :: t a -> [a]
  toList_ = foldr_ (:) []

  null_ :: t a -> Bool
  null_ = foldr_ (\_ _ -> False) True

  length_ :: t a -> Int
  length_ = foldr_ (\_ n -> n + 1) 0

  elem_ :: Eq a => a -> t a -> Bool
  elem_ e = foldr_ (\v a -> a || (e == v)) False

  maximum_ :: forall a. Ord a => t a -> a

  minimum_ :: forall a. Ord a => t a -> a

  sum_ :: Num a => t a -> a
  sum_ = unSum_ . foldMap_ Sum_

  product_ :: Num a => t a -> a
  product_ = unProd_ . foldMap_ Prod_
