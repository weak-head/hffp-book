module ChEx where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid

{-

1. []
pure  :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]

2. IO
pure  :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

3. (,) a
pure  :: a -> (monoid,a)
(<*>) :: (e, a -> b) -> (e, a) -> (e, b)

4. (->) a
pure  :: a -> b -> a
(<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
(<*>) f g x = f x (g x)

-}

----------------------------------------------------------------------

data Pair a =
  Pair a a
  deriving (Show, Eq)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure x = Pair x x
  (<*>) (Pair f1 f2) (Pair x1 x2) = Pair (f1 x1) (f2 x2)

instance Arbitrary x => Arbitrary (Pair x) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

pairChecker = quickBatch $ applicative a
  where
    a :: Pair (Int, String, Bool)
    a = undefined

----------------------------------------------------------------------

data Two a b =
  Two a b
  deriving (Show, Eq)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (<*>) (Two s f) (Two s' v) = Two (s <> s') (f v)

instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b

twoChecker = quickBatch $ applicative a
  where
    a :: Two String (Int, String, Bool)
    a = undefined

----------------------------------------------------------------------
