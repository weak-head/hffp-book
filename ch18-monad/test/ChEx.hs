module ChEx where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = parallel $ do
  describe "Nope" $ do
    testBatch (functor (undefined :: Nope (Int, Int, Int)))
    testBatch (applicative (undefined :: Nope (Int, Bool, String)))
    testBatch (monad (undefined :: Nope (Int, String, String)))
  describe "MEither" $ do
    testBatch (functor (undefined :: MEither Int (Int, Int, Int)))
    testBatch (applicative (undefined :: MEither String (Int, Bool, String)))
    testBatch (monad (undefined :: MEither Bool (Int, String, String)))


--- Nope instance ----------------------------------------------------

data Nope a =
  NopeDotJpg
  deriving (Show, Eq)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  return = pure
  (>>=) _ _ = NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

--- Either instance --------------------------------------------------

data MEither b a =
    MLeft a
  | MRight b
  deriving (Show, Eq)

instance Functor (MEither b) where
  fmap _ (MRight b) = MRight b
  fmap f (MLeft a)  = MLeft (f a)

instance Applicative (MEither b) where
  pure = MLeft
  (<*>) (MRight b) _ = MRight b
  (<*>) _ (MRight b) = MRight b
  (<*>) (MLeft f) (MLeft v) = MLeft (f v)

instance Monad (MEither b) where
  return = pure
  (>>=) (MRight b) _ = MRight b
  (>>=) (MLeft a) f = f a

instance (Eq a, Eq b) => EqProp (MEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (MEither b a) where
  arbitrary =
    frequency [ (1, MLeft <$> arbitrary)
              , (1, MRight <$> arbitrary) ]
