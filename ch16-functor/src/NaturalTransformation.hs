{-# LANGUAGE RankNTypes #-}

module NaturalTransformation where

-- transforming the structure preserving the values as is
type Nat f g = forall a . f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]



type ExplicitNat f g a = f a -> g a

exMaybeToList :: ExplicitNat Maybe [] a
exMaybeToList Nothing = []
exMaybeToList (Just a) = [a]


-- really bad idea
-- this is not a natural transformation
degenerateMtl :: Num a => ExplicitNat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]
