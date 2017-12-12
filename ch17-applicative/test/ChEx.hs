module ChEx where

{-

1. []
pure  :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]

2. IO
pure  :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b

3. (,) a
pure  :: a -> (monoid,a)
(<*>) :: (e, a -> b) -> (e, a) -> (e, b)

4. (->) a
pure  :: a -> b -> a
(<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)
(<*>) f g x = f x (g x)

-}
