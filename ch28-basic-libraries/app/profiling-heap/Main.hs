module Main where

import Control.Monad

blah :: [Integer]
blah = [1..1000]


--
-- > stack ghc -- -prof -fprof-auto -rtsopts -O2 Main.hs
-- > ./Main +RTS -hc -p
-- > hp2ps Main.hp
--
main :: IO ()
main = replicateM_ 10000 (print blah)

