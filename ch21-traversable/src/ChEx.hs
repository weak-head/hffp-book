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
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

constantQB = do
  quickBatch (functor (undefined :: Constant Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))

--- Maybe ------------------------------------------------------------

data Opt a =
    N
  | Y a
  deriving (Show, Eq, Ord)

instance Functor Opt where
  fmap _  N    = N
  fmap f (Y a) = Y $ f a

instance Foldable Opt where
  foldr _ d  N    = d
  foldr f d (Y v) = f v d

instance Traversable Opt where
  traverse _  N    = pure N
  traverse f (Y v) = Y <$> f v

instance Arbitrary a => Arbitrary (Opt a) where
  arbitrary =
    frequency [ (1, return N)
              , (3, Y <$> arbitrary)
              ]

instance Eq a => EqProp (Opt a) where
  (=-=) = eq

maybeQB = do
  quickBatch (functor (undefined :: Opt (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Opt (Int, Int, [Int])))
