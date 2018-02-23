module Main where

{-

newtype Seq a = Seq (FingerTree (Elem a))

newtype Elem a = Elem { getElem :: a }

data FingerTree a
    = Empty
    | Single a
    | Deep {-# UNPACK #-} !Int !(Digit a)
           (FingerTree (Node a)) !(Digit a)

-}

import Criterion.Main
import qualified Data.Sequence as S

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs =
  replicate 10 (S.fromList [1..100000])

ll :: [Int]
ll = [1..100000]

ss :: S.Seq Int
ss = S.fromList [1..100000]

main :: IO ()
main = defaultMain
  [ bench "concatenate lists" $
    nf mconcat lists
  , bench "concatenate sequence" $
    nf mconcat seqs
    ---
  , bench "indexing list" $
    whnf (\xs -> xs !! 9001) ll
  , bench "indexing sequence" $
    whnf (flip S.index 9001) ss
  ]
