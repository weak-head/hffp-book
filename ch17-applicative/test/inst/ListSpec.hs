module ListSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers

-- | Defines a linked list
data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

-- | Right associative fold of a 'List'
foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' _ v Nil        = v
foldr' f v (Cons h t) = f h (foldr' f v t)

-- | Append two 'List'
con' :: List a -> List a -> List a
con' Nil xs = xs
con' xs Nil = xs
con' (Cons h t) xs = Cons h (con' t xs)

-- | The concatenation of all elements of a 'List' of 'List'
concat' :: List (List a) -> List a
concat' = foldr' con' Nil

-- | Map a function over all the elements of a 'List' and concatenate the resulting 'List'
concatMap' :: (a -> List b) -> List a -> List b
concatMap' f = concat' . fmap f

-- | The 'List' functor
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- | The 'List' functor with application
instance Applicative List where
  pure x      = Cons x Nil
  (<*>) fs xs = concatMap' ($xs) (fmap fmap fs)
