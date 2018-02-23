module Main where

{-

data Vector a =
     Vector {-# UNPACK #-} !Int
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !(Array a)
     deriving ( Typeable )

-}

import Data.Vector ((//))
import Criterion.Main
import qualified Data.Vector as V

----------------------------------------

slice :: Int -> Int -> [a] -> [a]
slice from len xs =
  take len (drop from xs)

l :: [Int]
l = [1..1000]

v :: V.Vector Int
v = V.fromList [1..1000]

----------------------------------------

testV' :: Int -> V.Vector Int
testV' n =
  V.map (+n) $ V.map (+n) $
  V.map (+n) $ V.map (+n) $
  V.fromList [1..10000]

testV :: Int -> V.Vector Int
testV n =
  V.map ((+n) . (+n) . (+n) . (+n)) $
  V.fromList [1..10000]

----------------------------------------

vec :: V.Vector Int
vec = V.fromList [1..10000]

slow :: Int -> V.Vector Int
slow n = go n vec
  where
    go 0 v = v
    go n v = go (n-1) (v // [(n, 0)])

batchList :: Int -> V.Vector Int
batchList n = vec // updates
  where
    updates = fmap (\k -> (k, 0)) [1..n]

main :: IO ()
main = defaultMain
  [ bench "slicing list"   $  whnf (head . slice 100 900) l
  , bench "slicing vector" $  whnf (V.head . V.slice 100 900) v
  ---
  , bench "vector map prefused" $ whnf testV 9998
  , bench "vector map will be fused" $ whnf testV' 9998
  ---
  , bench "slow" $ whnf slow 9998
  , bench "batch list" $ whnf batchList 9998
  ]

