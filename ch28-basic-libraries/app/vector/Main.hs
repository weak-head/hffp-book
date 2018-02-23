module Main where

{-

data Vector a =
     Vector {-# UNPACK #-} !Int
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !(Array a)
     deriving ( Typeable )

-}

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

main :: IO ()
main = defaultMain
  [ bench "slicing list"   $  whnf (head . slice 100 900) l
  , bench "slicing vector" $  whnf (V.head . V.slice 100 900) v
  , bench "vector map prefused" $ whnf testV 9998
  , bench "vector map will be fused" $ whnf testV' 9998
  ]

