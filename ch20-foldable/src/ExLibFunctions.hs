module ExLibFunctions where

import Data.Monoid
import Data.Foldable

sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\v a -> a || (v == x)) False

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr stmin Nothing
  where stmin v Nothing  = Just v
        stmin v (Just k) = Just $ min v k

maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr stmax Nothing
  where stmax v Nothing = Just v
        stmax v (Just k) = Just $ max v k

null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

length' :: (Foldable t) => t a -> Int
length' = foldr (\_ n -> n + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (:) []

fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\v a -> f v <> a) mempty
