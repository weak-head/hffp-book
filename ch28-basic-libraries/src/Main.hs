module Main where

-- > stack ghc -- -O2 bench.hs
-- or
-- > ghc -O2 bench.hs

import Criterion.Main
import Debug.Trace

----------------------------------------

infixl 9 !?
-- Defaults to Integer, the Int optimization is what makes the difference.
-- (!?) :: [a] -> Int -> Maybe a
_      !? n | n < 0 = Nothing
[]     !? _         = Nothing
(x:_)  !? 0         = Just x
(_:xs) !? n         = xs !? (n - 1)

----------------------------------------

infixl 9 !??
{-# INLINABLE (!??) #-}
-- (!??) :: [a] -> Int -> Maybe a
xs !?? n
  | n < 0     = Nothing
  | otherwise =
      foldr (\x r k -> case k of
                         0 -> Just x
                         _ -> r (k-1))
      (const Nothing) xs n

----------------------------------------

myList' :: [Int]
myList' = trace "myList' was evaluated" ([1..9999] ++ [undefined])

myList :: [Int]
myList = [1..9999]

-- stack ghc -- -O2 ./src/Main.hs
-- ./src/Main
main :: IO ()
main = defaultMain
  [ bench "map list whnf 9999" $ whnf (map (+1)) myList -- guarded recursion
  , bench "map list nf 9999" $ nf (map (+1)) myList
  --
  , bench "index list !!  9999" $ whnf (myList !!)  9998
  , bench "index list !?  9999" $ whnf (myList !?)  9998
  , bench "index list !?? 9999" $ whnf (myList !??) 9998
  --
  --
  , bench "index nf mlist !! 9999" $ nf (myList' !!) 9998 -- ok
  --
  , bench "index whnf mlist !! 9999" $ whnf (myList' !!) 9999 -- undefined
  , bench "index nf mlist !! 9999"   $ nf   (myList' !!) 9999 -- undefined
  --
  --
  --
  ]

-- > (Just undefined) `seq` 1
-- 1
--
-- > (\_ -> undefined) `seq` 1
-- 1
--
-- > ((\_ -> Just undefined) 0) `seq` 1
-- 1
--
-- > ((\_ -> undefined) 0) `seq` 1
-- exception
