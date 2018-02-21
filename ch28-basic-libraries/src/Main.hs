module Main where

-- > stack ghc -- -O2 bench.hs
-- or
-- > ghc -O2 bench.hs

import Criterion.Main

infixl 9 !?
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n - 1)

myList :: [Int]
myList = [1..9999]

-- stack ghc -- -O2 ./src/Main.hs
-- ./src/Main
main :: IO ()
main = defaultMain
  [ bench "index list 9999" $ whnf (myList !!) 9998
  , bench "index list maybe index 9999" $ whnf (myList !?) 9998
  ]