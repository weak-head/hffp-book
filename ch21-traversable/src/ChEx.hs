{-# LANGUAGE FlexibleContexts #-}
module ChEx where

import Data.Monoid ((<>))
import Control.Applicative (liftA2, liftA3)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

--- Identity ---------------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity $ f x

instance Foldable Identity where
  foldr f d (Identity x) = f x d

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
  (=-=) = eq

identityQB = do
  quickBatch (functor (undefined :: Identity (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))

--- Constant ---------------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldr _ d _ = d

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure $ Constant a

instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = Constant <$> arbitrary

instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq

constantQB = do
  quickBatch (functor (undefined :: Constant Int (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))

--- Maybe ------------------------------------------------------------

data Opt a =
    N
  | Y a
  deriving (Show, Eq, Ord)

instance Functor Opt where
  fmap _  N    = N
  fmap f (Y a) = Y $ f a

instance Foldable Opt where
  foldr _ d  N    = d
  foldr f d (Y v) = f v d

instance Traversable Opt where
  traverse _  N    = pure N
  traverse f (Y v) = Y <$> f v

instance Arbitrary a => Arbitrary (Opt a) where
  arbitrary =
    frequency [ (1, return N)
              , (3, Y <$> arbitrary)
              ]

instance Eq a => EqProp (Opt a) where
  (=-=) = eq

maybeQB = do
  quickBatch (functor (undefined :: Opt (Int, Int, [Int])))
  quickBatch (traversable (undefined :: Opt (Int, Int, [Int])))

--- List -------------------------------------------------------------

data List a =
    Nil
  | Cons a (List a)
  deriving (Show, Eq)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldr _ d Nil = d
  foldr f d (Cons x xs) = f x (foldr f d xs)

instance Traversable List where
  traverse  = (sequenceA .) . fmap
  sequenceA = foldr (liftA2 Cons) (pure Nil)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = fromL <$> arbitrary
    where fromL = foldr Cons Nil :: ([a] -> List a)

instance Eq a => EqProp (List a) where
  (=-=) xs ys =
    let xs' = take 200 $ toL xs
        ys' = take 200 $ toL ys
    in xs' `eq` ys'
    where toL = foldr (:) []

listQB = do
  quickBatch (functor (undefined :: List (Int, Int, [Int])))
  quickBatch (traversable (undefined :: List (Int, Int, [Int])))

--- Three ------------------------------------------------------------

data Three a b c =
  Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldr f d (Three _ _ c) = f c d

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = liftA3 Three arbitrary arbitrary arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq

threeQB = do
  quickBatch (functor (undefined :: Three Int String (Int, Bool, [String])))
  quickBatch (traversable (undefined :: Three Int String (Int, Bool, [String])))

--- Pair -------------------------------------------------------------

data Pair a b =
  Pair a b
  deriving (Show, Eq)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldr f d (Pair _ b) = f b d

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = liftA2 Pair arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

pairQB = do
  quickBatch (functor (undefined :: Pair String (Int, Bool, [String])))
  quickBatch (traversable (undefined :: Pair String (Int, Bool, [String])))

--- Big --------------------------------------------------------------

data Big a b =
  Big a b b
  deriving (Show, Eq)

instance Functor (Big a) where
  fmap f (Big a b c) = Big a (f b) (f c)

instance Foldable (Big a) where
  foldMap f (Big _ b c) = f b `mappend` f c

instance Traversable (Big a) where
  sequenceA (Big a fb fc) = liftA2 (Big a) fb fc

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = liftA3 Big arbitrary arbitrary arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
  (=-=) = eq

bigQB = do
  quickBatch (functor (undefined :: Big String (Int, Bool, [String])))
  quickBatch (traversable (undefined :: Big String (Int, Bool, [String])))

--- Bigger -----------------------------------------------------------

data Bigger a b =
  Bigger a b b b
  deriving (Show, Eq)

instance Functor (Bigger a) where
  fmap f (Bigger a b c d) = Bigger a (f b) (f c) (f d)

instance Foldable (Bigger a) where
  foldMap f (Bigger _ b c d) = f b <> f c <> f d

instance Traversable (Bigger a) where
  sequenceA (Bigger a fb fc fd) = liftA3 (Bigger a) fb fc fd

instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

biggerQB = do
  quickBatch (functor (undefined :: Bigger String (Int, Bool, [String])))
  quickBatch (traversable (undefined :: Bigger String (Int, Bool, [String])))

--- S ----------------------------------------------------------------

data S n a =
  S (n a) a
  deriving (Eq, Show)

instance Functor n => Functor (S n) where
  fmap f (S m a) = S (fmap f m) (f a)

instance Foldable n => Foldable (S n) where
  foldMap f (S m a) = foldMap f m <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S m a) = liftA2 S (traverse f m) (f a)

instance ( Functor m
         , Arbitrary a
         , Arbitrary (m a) )
        => Arbitrary (S m a) where
  arbitrary = liftA2 S arbitrary arbitrary

instance ( Eq a
         , Eq (m a))
        => EqProp (S m a) where
  (=-=) = eq

sSm = sample' (arbitrary :: Gen (S [] Int))

sQB = do
  quickBatch (functor (undefined :: S Maybe (Int, Bool, [String])))
  quickBatch (traversable (undefined :: S Maybe (Int, Bool, [String])))

--- Tree -------------------------------------------------------------

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

instance Functor Tree where
  fmap _ Empty        = Empty
  fmap f (Leaf a)     = Leaf $ f a
  fmap f (Node l a r) = Node (f <$> l) (f a) (f <$> r)

instance Foldable Tree where
  foldMap _ Empty        = mempty
  foldMap f (Leaf a)     = f a
  foldMap f (Node l a r) = foldMap f l <> f a <> foldMap f r

instance Traversable Tree where
  traverse _ Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node l a r) = liftA3 Node (traverse f l) (f a) (traverse f r)

instance Eq a => EqProp (Tree a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary =
    frequency [ (1, return Empty)
              , (2, Leaf <$> arbitrary)
              , (2, liftA3 Node arbitrary arbitrary arbitrary)
              ]

treeSm = sample' (arbitrary :: Gen (Tree Int))

treeQB = do
  quickBatch (functor (undefined :: Tree (Int, Bool, [String])))
  quickBatch (traversable (undefined :: Tree (Int, Bool, [String])))
