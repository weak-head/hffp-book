module ZipListApplicativeSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' n (Cons x xs)
  | n > 0     = Cons x (take' (n-1) xs)
  | otherwise = Nil
take' _ Nil = Nil

foldr' :: (a -> b -> b) -> b -> List a -> b
foldr' _ v Nil         = v
foldr' f v (Cons x xs) = f x (foldr' f v xs)

conc' :: List a -> List a -> List a
conc' Nil x = x
conc' (Cons x xs) ys = Cons x (conc' xs ys)

flat' :: List (List a) -> List a
flat' = foldr' conc' Nil

flatMap' :: (a -> List b) -> List a -> List b
flatMap' f = flat' . fmap f

instance Functor List where
  fmap _ Nil         = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x      = Cons x Nil
  (<*>) fs xs = undefined


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
