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

--- Constant ---------------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ d _ = d

instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

constantQB = do
  quickBatch (functor (undefined :: Constant Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))
