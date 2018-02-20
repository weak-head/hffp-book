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
--- Prevent sharing --------------------

f :: a -> Int
f _ = trace "f" 1
-- > f 'a'
-- f
-- 1
-- > f 'a'
-- f
-- 1

----------------------------------------

ca :: Int
ca = let a = trace "a" 2 + 2
     in (a + a)
-- > ca
-- a
-- 8
-- > ca
-- 8


cf :: Int
cf = (trace "a" 2 + 2)
   + (trace "a" 2 + 2)
-- > cf
-- a
-- a
-- 8
-- > cf
-- 8

----------------------------------------

ff :: a -> Int
ff = trace "f" const 1 -- eta reduction
-- > ff 'a'
-- f
-- 1
-- > ff 'a'
-- 1
-- > ff 'b'
-- 1

fg :: a -> Int
fg v = trace "f" const 1 v
-- > fg 'a'
-- f
-- 1
-- > fg 'a'
-- f
-- 1
-- > fg 'b'
-- f
-- 1

----------------------------------------

-- > let blah = Just 1
-- > fmap ((+1) :: Int -> Int) blah
-- Just 2
--
-- > :sprint blah
-- blah = _
--
-- > :t blah
-- blah :: Num a => Maybe a


-- > let bl = Just 1
-- > :t bl
-- bl :: Num a => Maybe a
--
-- > :sprint bl
-- bl = _
--
-- > fmap (+1) bl
-- Just 2
--
-- > :sprint bl
-- bl = _


-- > let fm = fmap (+1) bl
-- > :t fm
-- fm :: Num b => Maybe b
--
-- > :sprint fm
-- fm = _
--
-- > fm
-- Just 2
--
-- :sprint fm
-- fm = _

----------------------------------------

-- > :set -XImplicitParams
-- > import Debug.Trace
-- > :{
-- > let add :: (?x :: Int) => Int
-- >     add = trace "add" 1 + ?x
-- > :}
--
-- > let ?x = 1 in add
-- add
-- 2
--
-- > let ?x = 1 in add
-- add
-- 2

----------------------------------------
----------------------------------------

-- > :{
-- > let blah = Just (trace "eval'd 1" 1)
-- > :}
--
-- > :sprint blah
-- blah = _
--
-- > :t blah
-- blah :: Num a => Maybe a
--
-- > fmap (+1) blah
-- Just eval'd 1
-- 2
--
-- > fmap (+1) blah
-- Just eval'd 1
-- 2
--
-- > :sprint blah
-- blah = _


-- > :{
-- > let blah =
-- >       Just (trace "eval'd 1" (1 :: Int)) --no typeclass constraint
-- > :}
--
-- > :sprint blah
-- blah = Just _
--
-- > fmap (+1) blah
-- Just eval'd 1
-- 2
--
-- > fmap (+1) blah
-- Just 2

----------------------------------------

-- > let poly = 1
-- > let conc = poly :: Int
--
-- > :sprint poly
-- poly = _
-- > :sprint conc
-- conc = _
--
-- > poly
-- 1
-- > conc
-- 1
--
-- > :sprint poly
-- poly = _
-- > :sprint conc
-- conc = 1


-----
-- polymorphic expressions cant be shared...
-----

polymorphic :: Num a => a
polymorphic = 1
-- polymorphic
-- polymorphic =
--             \ @ a1_aRN $dNum_aRP ->
--               fromInteger $dNum_aRP (__integer 1)

concrete :: Int
concrete = 1
-- concrete
-- concrete = I# 1
