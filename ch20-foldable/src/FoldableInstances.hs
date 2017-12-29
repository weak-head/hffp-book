module FoldableInstances where

import Data.Monoid
import Data.Foldable

--- Identity ---------------------------------------------------------

newtype Identity a =
  Identity a

instance Foldable Identity where
  foldr f z (Identity x) = f x z
  foldl f z (Identity x) = f z x
  foldMap f (Identity x) = f x

fi1 = foldr (*) 1 (Identity 5) -- 5
fi2 = foldr (*) 5 (Identity 5) -- 25

fim = foldMap (*5) (Identity 100) :: Product Integer
