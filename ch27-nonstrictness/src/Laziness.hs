module Laziness where

--
-- example of non strict evaluation
--

ns1 = fst (1, undefined)        -- 1
ns2 = snd (undefined, 2)        -- 2

{-

Strict:    inside out;
Nonstrict: outside in;

-}

possiblyKaboom ::  (((a1, b1) -> a1) -> ((a, b) -> b) -> (Integer, t) -> t1) -> t1
possiblyKaboom =
  \f -> f fst snd (0, undefined)

true' :: a -> a -> a
true' = \a -> (\b -> a)

false' :: a -> a -> a
false' = \a -> (\b -> b)


pkTrue  = possiblyKaboom true'  -- 0
pkFalse = possiblyKaboom false' -- undefined

{-

1. (\f -> f fst snd (0, undefined)) (\a -> (\b -> a))

2. (\a -> (\b -> a)) fst snd (0, undefined)

3. (\b -> fst) snd (0, undefined)

4. fst (0, undefined)

-}

possibleKaboom' b =
  case b of
    True  -> fst tup
    False -> snd tup
  where tup = (0, undefined)

