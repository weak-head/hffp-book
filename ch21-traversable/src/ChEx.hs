module ChEx where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--- Identity ---------------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldr f d (Identity x) = f x d

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityQB = do
  quickBatch (functor (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
