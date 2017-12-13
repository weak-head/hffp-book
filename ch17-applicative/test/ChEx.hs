module ChEx where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

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
