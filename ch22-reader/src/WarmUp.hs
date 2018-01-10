module RW where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = liftA2 (,) z' z'

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' a Nothing  = a
fromMaybe' _ (Just a) = a

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
    -- Just [3, 2, 1]
  print $ sequenceA [x, y]
    -- [[1,4], [1,5], [1,6], [2,4] ..]
  print $ sequenceA [xs, ys]
    -- Just [6, 9]
  print $ summed <$> ((,) <$> xs <*> ys)
    -- Just 15
  print $ fmap summed ((,) <$> xs <*> zs)
    -- Nothing
  print $ bolt 7
    -- True
  print $ fmap bolt z
    -- [True, False, False]
------
  print $ sequenceA [(>3), (<8), even] 7
    -- t (Reader Int Bool) -> Reader (Int -> t Bool)
    -- [True, True, False]
------
  print $ foldr (&&) True $ sequA 7
  print $ and . sequA $ 7
------
  print $ fromMaybe' [] (sequA <$> s')
------
  print $ fromMaybe' True $ bolt <$> ys

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(>3), (<8), even]

s' = summed <$> ((,) <$> xs <*> ys)
