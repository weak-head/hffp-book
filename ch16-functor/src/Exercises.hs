{-# LANGUAGE TypeApplications #-}

module Exercises where

ex1 = (+1) <$> read @[Int] "[1]"

ex2 = (fmap . fmap) (++ "aba") (Just ["Hi, ", "Hello "])

ex3 = fmap (*2) (\x -> x - 2)

ex4 = ((return '1' ++ ) . show) <$> (\x -> [x, 1..3])

ex5 :: IO Integer
ex5 = let ioi = readIO @Integer "1"
          changed = fmap (read . ("123" ++) . show) ioi
      in fmap (*3) changed


----------------------------------------------------------------------

newtype Identity a = Identity a
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

----------------------------------------------------------------------
