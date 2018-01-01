module Instances where

import Data.Monoid
import Control.Applicative (liftA2)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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

instance (Arbitrary a, Arbitrary b) => Arbitrary (E a b) where
  arbitrary =
    frequency [ (1, L <$> arbitrary)
              , (1, R <$> arbitrary) ]

instance (Eq a, Eq b) => EqProp (E a b) where
  (=-=) = eq

qCoProductBatch = do
  let sut = (undefined :: E (Int) (Int, Int, [Int]))
  quickBatch (functor sut)
  quickBatch (applicative sut)
  quickBatch (monad sut)
  quickBatch (traversable sut)


--- Product ----------------------------------------------------------

data P a b =
  P a b
  deriving (Show, Eq, Ord)

instance Functor (P a) where
  fmap f (P a b) = P a (f b)

instance Monoid a => Applicative (P a) where
  pure x = P mempty x
  (<*>) (P s f) (P s' v) = P (s `mappend` s') (f v)

instance Monoid a => Monad (P a) where
  return = pure
  (>>=) (P s v) f = let (P s' v') = f v
                    in P (s `mappend` s') v'

instance Foldable (P a) where
  foldMap f (P _ v) = f v
  foldr f d (P _ v) = f v d

instance Traversable (P a) where
  traverse f (P a b) = fmap (P a) (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (P a b) where
  arbitrary = liftA2 P arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (P a b) where
  (=-=) = eq

qProductBatch = do
  let sut = (undefined :: P (Sum Int) (Int, Int, [Int]))
  quickBatch (functor sut)
  quickBatch (applicative sut)
  quickBatch (monad sut)
  quickBatch (traversable sut)
