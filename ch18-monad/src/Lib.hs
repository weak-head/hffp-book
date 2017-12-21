module Lib
    ( someFunc
    ) where

import Control.Monad (join)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- class Applicative m => Monad m where
-- (>>=)  :: m a -> (a -> m b) -> m b
-- (>>)   :: m a -> m b -> m b
-- return :: a -> m a
--
-- fmap f xs = xs >>= return . f
--
-- > [1..3] >>= return . (+1)
-- [2,3,4]


-- fmap :: Functor f
--      => (a -> b) -> f a -> f b
--
-- <*>  :: Applicative f
--      => f (a -> b) -> f a -> f b
--
-- >>=  :: Monad f
--      => f a -> (a -> f b) -> f b
--
--
-- ~ if b == f b
-- fmap :: Functor f
--      => (a -> f b) -> f a -> f (f b)
--


-- > let andOne x = [x, 1]
-- > andOne 10
-- [10,1]
--
-- > :t fmap andOne [4, 5, 6]
-- fmap andOne [4, 5, 6] :: Num t => [[t]]
--
-- > fmap andOne [4, 5, 6]
-- [[4,1],[5,1],[6,1]]
--
-- > concat $ fmap andONe [4, 5, 6]
-- [4,1,5,1,6,1]

-- import Control.Monad (join)
-- join :: Monad m => m (m a) -> m a


-- (>>=)
bind' :: Monad m => (a -> m b) -> m a -> m b
bind' f = join . fmap f
