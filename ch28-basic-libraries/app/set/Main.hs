module Main where

{-

data Set a
  = Bin
    {-# UNPACK #-}
    !Size !a !(Set a) !(Set a)
  | Tip

type Size = Int

-}

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where
    stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where
    stream = iterate (+1) 0

main :: IO ()
main = print ""
