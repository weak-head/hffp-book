module Lift where

-- kind of fmap:
-- liftA :: Applicative f => (a  -> b) -> f a  -> f b
-- liftM :: Monad m       => (a1 -> r) -> m a1 -> m r


-- liftA2 :: Applicative f
--        => (a  -> b  -> c) -> f a  -> f b  -> f c
-- liftM2 :: Monad m
--        => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
--
-- > liftA2 (,) (Just 3) (Just 5)
-- Just (3, 5)
--
-- > liftM2 (,) (Just 3) (Just 5)
-- Just (3, 5)
--
--
-- > :t zipWith
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
--
-- > zipWith (+) [3, 4] [5, 6]
-- [8, 10]
--
-- > liftA2 (+) [3, 4] [5, 6]
-- [8, 9, 9, 10]



-- liftA3 :: Applicative f
--        => (a -> b -> c -> d)
--        -> f a -> f b -> f c -> f d
-- liftM3 :: Monad m
--        => (a1 -> a2 -> a3 -> r)
--        -> m a1 -> m a2 -> m a3 -> m r
--
-- > :t zipWith3
-- zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
--
-- > liftM3 (,,) [1, 2] [3] [5, 6]
-- [(1,3,5),(1,3,6),(2,3,5),(2,3,6)]
--
-- > zipWith3 (,,) [1, 2] [3] [5, 6]
-- [(1,3,5)]
