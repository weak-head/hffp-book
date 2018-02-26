module DiffList where

import Criterion.Main

-- Difference list
newtype DList a = DL { unDL :: [a] -> [a] }

----------------------------------------

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton = DL . (:)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList (DL f) = f []
{-# INLINE toList #-}

-- prepend
infix `cons`
cons :: a -> DList a -> DList a
cons x xs = DL $ (:) x . unDL xs
{-# INLINE cons #-}

-- append
infixl `snoc`
snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (:) x
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs ys = DL $ unDL xs . unDL ys
{-# INLINE append #-}

----------------------------------------

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where go 0 xs = xs
        go n xs = go (n-1) ([n] ++ xs)

constructDlist :: Int -> [Int]
constructDlist i = toList $ go i empty
  where go 0 xs = xs
        go n xs = go (n - 1) (singleton n `append` xs)

benchDl :: IO ()
benchDl = defaultMain
  [ bench "concat list"  $ whnf schlemiel 123456
  , bench "concat dlist" $ whnf constructDlist 123456
  ]
