module Wrap where

-- intemediate type layer
newtype Wrap f a =
  Wrap (f a)
  deriving (Show, Eq)

-- functor instance for the intermediate layer
instance (Functor f) => Functor (Wrap f) where
  fmap f (Wrap m) = Wrap (fmap f m)


fw1 = fmap (+1) (Wrap (Just 1))

fw2 = fmap (+1) (Wrap [1..5])

-- wont work
-- fmap (+1) (Wrap 1)
