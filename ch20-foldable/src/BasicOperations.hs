module BasicOperations where

import Data.Monoid
import Data.Foldable

--- toList -------------------

l1 = toList (Just 1)                            -- [1]
l2 = map toList [Just 1, Just 2, Just 3]        -- [[1], [2], [3]]

l3 = concatMap toList [Just 1, Just 2, Just 3]  -- [1,2,3]
l4 = concatMap toList [Just 1, Just 2, Nothing] -- [1,2]

l5 = toList (1, 2)                              -- [2]

--- null ---------------------

n1 = null (Left 3)                       -- True
n2 = null []                             -- True
n3 = null Nothing                        -- True
n4 = null (1, 2)                         -- False
n5 = fmap null [Just 1, Just 2, Nothing] -- [False, False, True]

--- length -------------------

le1 = length (1, 2)                          -- 1
le2 = length [(1, 2), (3, 4), (5, 6)]        -- 3
le3 = fmap length [(1, 2), (3, 4), (5, 6)]   -- [1,1,1]

le4 = length $ Just [1,2,3]                  -- 1
le5 = fmap length Just [1, 2, 3]             -- 1
le6 = fmap length Just undefined             -- 1
le7 = fmap length Just $ [1, 2, 3]           -- 1
le8 = fmap length (Just [1, 2, 3])           -- Just 3
le9 = length <$> Just [1, 2, 3]              -- Just 3
le0 = fmap (fmap length) Just [1, 2, 3]      -- Just 3

le10 = fmap length [[1,2,3]]                 -- [3]
le11 = fmap length [Just 1, Just 2, Just 3]  -- [1,1,1]
le12 = fmap length [Just 1, Just 2, Nothing] -- [1,1,0]

--- elem ---------------------

e1 = elem 2 (Just 3)         -- False
e2 = elem True (Left False)  -- False
e3 = elem True (Left True)   -- False
e4 = elem True (Right False) -- False
e5 = elem True (Right True)  -- True

e6 = fmap (elem 3) [Right 1, Right 2, Right 3] -- [False, True, True]

--- maximum & minimum --------

m1 = maximum [10, 12, 33]                   -- 33
m2 = fmap maximum [Just 2, Just 10, Just 4] -- [2.10,4]
m3 = fmap maximum (Just [3, 7, 10, 2])      -- Just 10
m4 = minimum "julie"                        -- e
m5 = fmap minimum (Just "julie")            -- Just 'e'
m6 = let l = map Just "jul"
     in fmap minimum l                      -- "jul"
m7 = fmap minimum [Just 4, Just 3, Nothing] -- Exception
m8 = minimum (Left 3) :: Either Int Int     -- Exception

--- sum & product ------------

s1 = sum (7, 5)                      -- 5
s2 = fmap sum [(7,5), (3,4)]         -- [5, 4]
s3 = fmap sum (Just [1, 2, 3, 4, 5]) -- Just 15

p1 = product Nothing              -- 1
p2 = fmap product (Just [])       -- Just 1
p3 = fmap product (Right [1,2,3]) -- Right 6
