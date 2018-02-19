module Sharing where

import Debug.Trace


----------------------------------------
a :: Integer
a = trace "a" 1

b :: Integer
b = trace "b" 2

c :: Integer
c = a + b
----------------------------------------

inc   = (+1)
twice = inc . inc

howManyTimes =
  inc (trace "I got eval'd A" (1 + 1))
    + twice (trace "I got eval'd B" (1 + 1))
-- > howManyTimes
-- I got eval'd B
-- I got eval'd A
-- 7

howManyTimes' =
  let onePlusOne =
        trace "I got eval'd C" (1 + 1)
  in inc onePlusOne + twice onePlusOne
-- > howManyTimes'
-- I got eval'd C
-- 7

----------------------------------------

x = trace "x" (1 :: Int)
y = trace "y" (1 :: Int)
xy = x + y
-- > xy
-- x
-- y
-- 2

z = trace "z" (1 :: Int)
zz = z + z
-- > zz
-- z
-- 2

idz = (id z) + (id z)
-- > idz
-- z
-- 2

----------------------------------------

la = Just ['a']
-- > let la = Just ['a']
-- > :sprint la
-- a = Just "a"
--
-- returnIO (: ((Just (: (C# 'a') ([]))) `cast` ...) ([]))


sa = Just "a"
-- > let sa = Just "a"
-- > :sprint sa
-- sa = Just _
--
-- returnIO (: ((Just (unpackCString# "a"#)) `cast` ...) ([]))

----------------------------------------
----------------------------------------
