{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Applicative

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- class Functor f => Applicative f where
--   -- identity
--   pure :: a -> f a
--   -- apply, tie fighter
--   (<*>) :: f (a -> b) -> f a -> f b

-- fmap
-- <$> :: Functor f => (a -> b) -> f a -> f b

-- <*> :: Applicative f => f (a -> b) -> f a -> f b


-- Control.Applicative
-- liftA :: Applicative f => (a -> b) -> f a -> f b
-- liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d


-- fmap f x = pure f <*> x
lf1 = fmap (+1) [1, 2, 3]
lf2 = pure (+1) <*> [1, 2, 3]

lfp1 = pure 1 :: [Int]        -- [1]
lfp2 = pure 1 :: Maybe Int    -- Just 1
lfp3 = pure 1 :: Either a Int -- Right 1
lfp4 = pure 1 :: ([a], Int)   -- ([], 1)
