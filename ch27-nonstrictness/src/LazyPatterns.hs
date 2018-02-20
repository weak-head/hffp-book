module LazyPatterns where

-------

strictPattern :: (a, b) -> String
strictPattern (a,b) = const "String" a

lazyPattern :: (a, b) -> String
lazyPattern ~(a,b) = const "String" a

--------

strict = strictPattern undefined
-- exception

lazy = lazyPattern undefined
-- "String"
