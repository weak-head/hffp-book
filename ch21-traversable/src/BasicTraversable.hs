module BasicTraversable where

import Data.Maybe

{-

class (Functor t, Foldable t) => Traversable t where
  traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  traverse f = sequenceA . fmap f

  sequenceA :: Applicative f => t (f a) -> f (t a)
  sequenceA = traverse id
  {-# MINIMAL traverse | sequenceA #-}

-}

--- sequenceA --------------------------------------------------------

-- > sum [1, 2, 3]
-- 6
-- > fmap sum [Just 1, Just 2, Just 3]
-- [1, 2, 3]
-- > (fmap . fmap) sum Just [1, 2, 3]
-- Just 6
-- > fmap product [Just 1, Just 2, Nothing]
-- [1, 2, 1]

ts1 = fmap Just [1, 2, 3] -- [Just 1, Just 2, Just 3]

ts2 = sequenceA $ fmap Just [1, 2, 3] -- traverse Just [1, 2, 3]
-- Just [1, 2, 3]

ts3 = sequenceA [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]

ts4 = sequenceA [Just 1, Just 2, Nothing]
-- Nothing

ts5 = fmap sum $ sequenceA [Just 1, Just 2, Just 3]
-- Just 6

ts6 = fmap product (sequenceA [Just 3, Just 4, Nothing])
-- Nothing


ts7 = catMaybes [Just 1, Just 2, Just 3]
-- [1, 2, 3]

ts8 = catMaybes [Just 1, Just 2, Nothing]
-- [1, 2]

ts9 = sum $ catMaybes [Just 1, Just 2, Just 3, Nothing]
-- 6

ts10 = fmap sum $ sequenceA [Just 1, Just 2, Just 3, Nothing]
-- Nothing
