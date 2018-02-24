module Main where

{-

data Vector a =
     Vector {-# UNPACK #-} !Int
            {-# UNPACK #-} !Int
            {-# UNPACK #-} !(Array a)
     deriving ( Typeable )

-}

import           Control.Monad.Primitive
import           Control.Monad.ST
import           Criterion.Main
import           Data.Vector ((//))
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable as MV

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
    updates = fmap (\k -> (k, 0)) [0..n]

batchVector :: Int -> V.Vector Int
batchVector n = V.unsafeUpdate vec updates
  where
    updates = fmap (\k -> (k, 0)) (V.fromList [0..n])

----------------------------------------

mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n+1)
  go n mvec
  where
    go 0 v = return v
    go n v = MV.write v n 0 >> go (n-1) v

mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n+1)
  go n mvec
  where
    go 0 v = V.freeze v
    go n v = MV.write v n  0 >> go (n-1) v

----------------------------------------

main :: IO ()
main = defaultMain
  [
    bgroup "Slicing" [
        bench "slicing list"   $  whnf (head . slice 100 900) l
      , bench "slicing vector" $  whnf (V.head . V.slice 100 900) v
      ]

  ---

  , bgroup "Stream fusion" [
        bench "vector map prefused" $ whnf testV 9998
      , bench "vector map will be fused" $ whnf testV' 9998
      ]

  ---

  , bgroup "Updates" [
        bgroup "Persistent vectors" [
            bench "slow" $ whnf slow 9998
          , bench "batch list" $ whnf batchList 9998
          , bench "batch vector" $ whnf batchVector 9998
          ]
      , bgroup "Mutable vectors" [
            bench "mutable IO vector" $ whnfIO $ mutableUpdateIO 9998
          , bench "mutable ST vector" $ whnf mutableUpdateST 9998
          ]
      ]

  ]
