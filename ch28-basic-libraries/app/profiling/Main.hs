module Main where

f :: IO ()
f = do
  print ([1..] !! 999999)
  putStrLn "f"

g :: IO ()
g = do
  print ([1..] !! 9999999)
  putStrLn "g"


--
-- > stack ghc -- -prof -fprof-auto -rtsopts -O2 Main.hs
-- > ./Main +RTS -P
-- > cat Main.prof
--
main :: IO ()
main = f >> g
