module ChEx where

import Test.Hspec
import Test.Hspec.Checkers
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Control.Applicative (liftA2)

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

  describe "Identity" $ do
    testBatch (functor (undefined :: Identity (Int, Int, Int)))
    testBatch (applicative (undefined :: Identity (Int, Bool, String)))
    testBatch (monad (undefined :: Identity (Int, String, String)))

  describe "List" $ do
    testBatch (functor (undefined :: List (Int, Int, Int)))
    testBatch (applicative (undefined :: List (Int, Bool, String)))
    testBatch (monad (undefined :: List (Int, String, String)))


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

--- Identity instance ------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity v) = Identity (f v)

instance Monad Identity where
  return = pure
  (>>=) (Identity v) f = f v

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

--- List instance ----------------------------------------------------

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

ltake :: Int -> List a -> List a
ltake _ Nil = Nil
ltake n (Cons x xs)
  | n <= 0 = Nil
  | otherwise = Cons x (ltake (n-1) xs)

lconc :: List a -> List a -> List a
lconc xs Nil = xs
lconc Nil xs = xs
lconc (Cons x xs) xs' = Cons x (lconc xs xs')

lfoldr :: (a -> b -> b) -> b -> List a -> b
lfoldr _ v Nil = v
lfoldr f v (Cons x xs) = f x (lfoldr f v xs)

lconcat :: List (List a) -> List a
lconcat = lfoldr lconc Nil

lconcatMap :: (a -> List b) -> List a -> List b
lconcatMap f = lconcat . fmap f

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  (<*>) fs xs = lconcatMap ($xs) (fmap fmap fs)

instance Monad List where
  return = pure
  (>>=) xs f = lconcatMap f xs

instance Eq a => EqProp (List a) where
  (=-=) xs ys = xs' `eq` ys'
    where
      xs' = ltake 300 xs
      ys' = ltake 300 ys

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = sized $ \x ->
    do n <- choose (0, min x 300)
       genList n arbitrary
    where
      genList c f
        | c <= 0 = pure Nil
        | otherwise = liftA2 Cons f (genList (c-1) f)
