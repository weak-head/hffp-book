module Main where

-- CAF
incdInts :: [Integer]
incdInts = map (+1) [1..]

-- not a CAF
incdInts' :: [Integer] -> [Integer]
incdInts' x = map (+1) x

-- CAF
incdInts'' :: [Integer] -> [Integer]
incdInts'' = map (+1)

--
-- > stack ghc -- -prof -fprof-auto -rtsopts -O2 Main.hs
-- > ./Main +RTS -P
-- > cat Main.prof
--
main :: IO ()
main = do
  print (incdInts !! 1000)
  print (incdInts !! 9001)
  print (incdInts !! 90010)
  print (incdInts !! 9001000)
  print (incdInts !! 9501000)
  print (incdInts !! 9901000)
  print (incdInts'  [1..] !! 1000)
  print (incdInts'' [1..] !! 1000)
