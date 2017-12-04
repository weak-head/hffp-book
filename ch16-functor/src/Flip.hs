{-# LANGUAGE FlexibleInstances #-}

module Flip where

data Tuple a b =
  Tuple a b
  deriving (Show, Eq)

newtype Flip f a b =
  Flip (f b a)
  deriving (Show, Eq)

-- that is not really nice
instance Functor (Flip Tuple a) where
  fmap f (Flip (Tuple a b)) = Flip (Tuple (f a) b)


ff1 = fmap (+1) (Flip (Tuple 1 "blah"))
