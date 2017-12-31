module Instances where

--- Either -----------------------------------------------------------

data E a b =
    L a
  | R b
  deriving (Eq, Ord, Show)

instance Functor (E a) where
  fmap _ (L a) = L a
  fmap f (R b) = R $ f b

instance Applicative (E a) where
  pure = R
  (<*>) (L e) _ = L e
  (<*>) (R f) v = fmap f v

instance Monad (E a) where
  return = R
  (>>=) (L e) _ = L e
  (>>=) (R v) f = f v

instance Foldable (E a) where
  foldMap _ (L _) = mempty
  foldMap f (R v) = f v

  foldr _ d (L _) = d
  foldr f d (R v) = f v d

instance Traversable (E a) where
  traverse _ (L e) = pure $ L e
  traverse f (R v) = fmap R (f v)
