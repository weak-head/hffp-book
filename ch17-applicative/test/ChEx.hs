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

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three a b f) (Three a' b' v) = Three (a <> a') (b <> b') (f v)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c

threeChecker = quickBatch $ applicative a
  where
    a :: Three String String (Int, String, Bool)
    a = undefined

----------------------------------------------------------------------

data Three' a b =
  Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' s f1 f2) (Three' s' v1 v2) = Three' (s <> s') (f1 v1) (f2 v2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three' a b c

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

threeCheck = quickBatch $ applicative a
  where
    a :: Three' String (Int, String, Bool)
    a = undefined

----------------------------------------------------------------------

data Four a b c d =
  Four a b c d
  deriving (Show, Eq)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four a b c f) (Four a' b' c' v) =
    Four (a <> a') (b <> b') (c <> c') (f v)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

fourCheck = quickBatch $ applicative a
  where
    a :: Four String String String (String, Int, Bool)
    a = undefined

----------------------------------------------------------------------

data Four' a b =
  Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' s1 s2 s3 f) (Four' s1' s2' s3' v) =
    Four' (s1 <> s1') (s2 <> s2') (s3 <> s3') (f v)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four' a b c d

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

fourCheck' = quickBatch $ applicative a
  where
    a :: Four' String (String, Int, Bool)
    a = undefined
