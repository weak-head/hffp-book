module Sand.List where

import Sand.ClassDef

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

infixr 5 ++!

(++!) :: List a -> List a -> List a
Nil         ++! xs = xs
(Cons x xs) ++! ys = Cons x (xs ++! ys)

-- join
concat_ :: List (List a) -> List a
concat_ = foldr_ (++!) Nil

instance Monoid_ (List a) where
  mempty_  = Nil
  mappend_ = (++!)

instance Functor_ List where
  fmap_ _ Nil         = Nil
  fmap_ f (Cons x xs) = Cons (f x) (fmap_ f xs)

instance Applicative_ List where
  pure_ x    = Cons x Nil
  fs <*>! xs = concat_ $ fmap_ ($xs) (fmap_ fmap_ fs)

instance Monad_ List where
  return_   = pure_
  xs >>=! f = concat_ $ fmap_ f xs

instance Foldable_ List where
  foldr_ _ d Nil         = d
  foldr_ f d (Cons x xs) = f x (foldr_ f d xs)

instance Traversable_ List where
  sequenceA_ = foldr_ (\v a -> fmap_ Cons v <*>! a) (pure_ Nil)
