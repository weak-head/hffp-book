module LawsSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = parallel $ do
  undefined

qmbatch = quickBatch (monad (undefined :: [(Int, Int, Int)]))

--- Bad Monad --------------------------------------------------------

data CountMe a =
  CountMe Integer a
  deriving (Eq, Show)

instance Functor CountMe where
  fmap f (CountMe i a) =
    -- CountMe i (f a) -- functor should not alter a state
    CountMe (i + 1) (f a)

instance Applicative CountMe where
  -- pure = CountMe 1 -- this is invalid Applicative
  pure = CountMe 0
  CountMe n f <*> CountMe n' a =
    -- CountMe (n +n') -- the correct implementation
    CountMe (n + n' + 1) (f a)

instance Monad CountMe where
  return = pure
  CountMe n a >>= f =
    let CountMe m b = f a
    -- in CountMe (n + m) b -- <- this is a valid Monad, has valid Monoid
    in CountMe (n + 1) b

instance Arbitrary a => Arbitrary (CountMe a) where
  arbitrary =
    CountMe <$> arbitrary <*> arbitrary

instance Eq a => EqProp (CountMe a) where
  (=-=) = eq

badTest = do
  let trigger :: CountMe (Int, String, Int)
      trigger = undefined
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
