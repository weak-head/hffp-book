module ZipListMonoidSpec where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck (arbitrary, Arbitrary)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

-- default monoid for lists
-- <>
-- [1, 2, 3] <> [4, 5, 6]
-- is concatenation
-- [1, 2, 3] ++ [4, 5, 6]
--
-- ZipList monoid combines values like this:
--
-- [1, 2, 3] <> [4, 5, 6]
-- [
--   1 <> 4
-- , 2 <> 5
-- , 3 <> 6
-- ]

-- Sum example
-- > 1 <> (2 :: Sum Integer)
-- Sum {getSum = 3}

instance Monoid a => Monoid (ZipList a) where
  mempty = ZipList marr
    where marr = mempty : marr
    -- (ZipList $ repeat mempty)
    -- pure mempty
  mappend = liftA2 mappend

-- instance Arbitrary a => Arbitrary (ZipList a) where
--   arbitrary = ZipList <$> arbitrary

-- instance Arbitrary a => Arbitrary (Sum a) where
--   arbitrary = Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where
  (=-=) = eq

zl = ZipList [1 :: Sum Int]
zlt = quickBatch $ monoid zl
