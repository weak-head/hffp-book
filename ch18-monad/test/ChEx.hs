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
