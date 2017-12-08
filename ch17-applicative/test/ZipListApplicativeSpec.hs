module ZipListApplicativeSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' = undefined

instance Functor List where
  fmap = undefined

instance Applicative List where
  pure = undefined
  (<*>) = undefined


newtype ZipList' a =
  ZipList' (List a)
  deriving (Show, Eq)


instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

-- | align a list of functions with a list of values
-- and apply the first function to the first value,
-- second function to the second value and so on...
instance Applicative ZipList' where
  pure = undefined
  (<*>) = undefined


---

repeat' x = Cons x (repeat' x)

toList' = foldr Cons Nil

zl' = ZipList'
z  = zl' $ toList' [(+9), (*2), (+8)]
z' = zl' $ toList' [1..3]
fz = z <*> z'
-- [10, 4, 11]

z'' = zl' $ (repeat' 1)
fz' = z <*> z''
-- [10, 2, 9]


instance Arbitrary a => Arbitrary (List a) where
  arbitrary = undefined

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

aplTst = quickBatch $ applicative a
  where
    a :: ZipList' (Int, String, Bool)
    a = undefined
