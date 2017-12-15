module ListSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
  pure x      = Cons x Nil
  (<*>) fs xs = undefined
