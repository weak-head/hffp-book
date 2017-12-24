module Composition where

import Control.Monad ((>=>))

-- fmap id = id
-- fmap f . fmap g = fmap (f . g)
-- > fmap ((+1) . (+2)) [1..5]
-- > fmap (+1) . fmap (+2) [1..5]

mcomp :: Monad m
      => (b -> m c)
      -> (a -> m b)
      -> a
      -> m c
-- this will now work:
-- > mcomp f g a = f (g a)
-- we can go with fmap and join
-- > mcomp f g a = join (f <$> (g a))
-- but semantically it's equal to
mcomp f g a = g a >>= f


-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- (>>=) :: Monad m => m a -> (a -> m b) -> m c

-- by flipping args

-- (>=>)    :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
-- flip (.) ::            (a ->   b) -> (b ->   c) -> a ->   c


sayHi :: String -> IO String
sayHi greeting = do
  putStrLn greeting
  getLine

readM :: Read a => String -> IO a
readM = return . read

getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge =
  getAge "Hello! How old are you? "
