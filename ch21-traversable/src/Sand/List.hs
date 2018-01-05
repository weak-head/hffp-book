module Sand.List where

import Sand.ClassDef

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

infixr 5 ++!
(++!) :: List a -> List a -> List a
(++!) = undefined

concat_ :: List (List a) -> List a
concat_ = undefined

instance Functor_ List where
  fmap_ _ Nil         = Nil
  fmap_ f (Cons x xs) = Cons (f x) (fmap_ f xs)

instance Foldable_ List where
  foldr_ _ d Nil         = d
  foldr_ f d (Cons x xs) = f x (foldr_ f d xs)
