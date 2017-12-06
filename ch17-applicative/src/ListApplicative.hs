{-# LANGUAGE TypeApplications #-}

module ListApplicative where

import Data.List (elemIndex)
import Control.Applicative

-- f ~ []
--
-- (<*>) :: f (a -> b) -> f a -> f b
-- ~
-- (<*>) :: [(a -> b)] -> [a] -> [b]
--
-- pure :: a -> f a
-- pure :: a -> [a]
--

-- > :type (<*>) @[]
-- (<*>) @[] :: [a -> b] -> [a] -> [b]
typedAp = (<*>) @[]

-- > :type pure @[]
-- pure @[] :: a -> [a]
typedPure = pure @[]

-- fmap (2^) [1, 2, 3] = [2, 4, 8]
-- fmap (^2) [1, 2, 3] = [1, 4, 9]
apList = [(+1), (*2)] <*> [2, 4] -- = [3, 5, 4, 8]


mApLst  = (,) <$> [1, 2] <*> [3, 4]
mApLst' = liftA2 (,) [1, 2] [3, 4]
--      = [(1,3), (1,4), (2,3), (2,4)]

sumLst  = (+) <$> [1, 2] <*> [3, 5]
sumLst' = liftA2 (+) [1, 2] [3, 5]
--      = [4, 6, 5, 7]

maxLst  = max <$> [1, 2] <*> [1, 4]
maxLst' = liftA2 max [1, 2] [1, 4]
--      = [1, 4, 2, 4]


-- > :t lookup
-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
--
-- > let l = lookup 3 [(3, "hello")]
-- > l
-- Just "hello"
--
-- > fmap length $ l
-- Just 5
--
-- > let c (x:xs) = toUpper x:xs
-- > fmap c $ l
-- Just "Hello"
--
-- > :m +Data.Map
-- > let m = fromList [(3, "hello")]
-- > fmap c $ Data.Map.lookup 3 m
-- Just "Hello"
--

f x =
  lookup x [ (3, "hello")
           , (4, "julie")
           , (5, "kbai")]

g y =
  lookup y [ (7, "sup?")
           , (8, "chris")
           , (9, "aloha")]

h z =
  lookup z [(2, 3), (5, 6), (7, 8)]

m x =
  lookup x [(4, 10), (8, 13), (1, 9001)]


-- > f 3
-- Just "hello"
--
-- > g 8
-- Just "chris"
--
-- > (++) <$> f 3 <*> g 7
-- Just "hellosup?"
--
-- > (+) <$> h 5 <*> m 1
-- Just 9007
--
-- > (+) <$> h 5 <*> m 6
-- Nothing
--
-- > liftA2 (++) (g 9) (f 4)
-- Just "alohajulie"
--
-- > liftA2 (^) (h 5) (m 4)
-- Just 60466176
--
-- > liftA2 (*) (h 5) (m 4)
-- Just 60
--
-- > liftA2 (*) (h 1) (m 1)
-- Nothing
--
-- > (++) <$> getLine <*> getLine
-- > (,) <$> getLine <*> getLine
--


added :: Maybe Integer
added = (+3) <$> lookup 3 (zip [1, 2, 3] [4, 5, 6])


y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = liftA2 (,) y z


xp :: Maybe Int
xp = elemIndex 3 [1, 2, 3, 4, 5]

yp :: Maybe Int
yp = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = liftA2 max' xp yp


xs = [1, 2, 3]
ys = [4, 5, 6]

xr :: Maybe Integer
xr = lookup 3 $ zip xs ys

yr :: Maybe Integer
yr = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> liftA2 (,) xr yr
