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
