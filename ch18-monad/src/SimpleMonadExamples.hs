module SimpleMonadExamples where

import Data.Bool

-------- List --------------------------------------------------------
-- (>>=) :: Monad m
--        => m  a -> (a ->  m  b) ->  m  b
-- (>>=) :: [ ] a -> (a -> [ ] b) -> [ ] b
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- return :: Monad m => a ->  m  a
-- return ::            a -> [ ] a
-- return ::            a -> [a]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEvenP :: [Integer] -> [Integer]
twiceWhenEvenP = (=<<) $ \x -> bool [x*x] [x*x, x*x] (even x)

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []
----------------------------------------------------------------------
