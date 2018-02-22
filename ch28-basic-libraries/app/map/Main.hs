module Main where

import Criterion.Main
import qualified Data.Map as M

{-

data Map k a
  = Bin
    {-# UNPACK #-}
    !Size !k a
    !(Map k a) !(Map k a)
  | Tip

type Size = Int

-}

genList :: Int -> [(String, Int)]
genList n = go n []
  where go 0  xs = ("0", 0) : xs
        go n' xs =
          go (n' - 1) ((show n', n') : xs)

pairList :: [(String, Int)]
pairList = genList 9001

testMap :: M.Map String Int
testMap = M.fromList pairList

main :: IO ()
main = defaultMain
    [
      bench "build up list" $
      nf genList 10000

    , bench "build up map" $
      nf (M.fromList . genList) 10000

      ------
    , bench "lookup one thing, list" $
      whnf (lookup "doesntExist") pairList

    , bench "lookup one more thing, map" $
      whnf (M.lookup "doesntExist") testMap
    ]
