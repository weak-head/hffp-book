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

--- Maybe ------------------------------------------------------------

fm1 = foldr (+) 1 Nothing -- 1
fm2 = foldMap (+1) Nothing :: Sum Integer -- 0

fm3 = foldr (+) 1 (Just 3) -- 4
fm4 = foldMap (+1) (Just 3) :: Sum Integer -- 4

data Optional x = Nada | Yep x deriving (Show, Eq)

instance Foldable Optional where
  foldr _ z Nada = z
  foldr f z (Yep x) = f x z

  foldl _ z Nada = z
  foldl f z (Yep x) = f z x

  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

frs1 = foldMap (+1) Nada :: Sum Int -- 0
frp1 = foldMap (+1) Nada :: Product Int -- 1
frs2 = foldMap (+1) (Yep 1) :: Sum Int -- 2
