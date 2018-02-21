module ChEx where

-- > let x = 1
-- > :sprint x
-- x = _
-- > x
-- 1
-- > :sprint x
-- x = _

-- > let x = ['1']
-- > :sprint x
-- x = "1"

-- > let x = [1]
-- > :sprint x
-- x = _

-- > let x = 1 :: Int
-- > :sprint x
-- x = 1

-- > let f = \x -> x
-- > let x = f 1
-- > :sprint f
-- f = _
-- > :sprint x
-- x = _

-- > let f :: Int -> Int; f = \x -> x
-- > let x = f 1
-- > :sprint f
-- f = _
-- > :sprint x
-- x = _
-- > x
-- 1
-- > :sprint x
-- x = 1

----------------------------------------

ex1 = snd (undefined, 1)
-- 1

ex2 = let x = undefined
          y = x `seq` 1
      in snd (x, y)
-- exception

ex3 = length $ [1..5] ++ undefined
-- exception

ex4 = length $ [1..5] ++ [undefined]
-- 6

ex5 = const 1 undefined
-- 1

ex6 = const 1 (undefined `seq` 1)
-- 1

ex7 = const undefined 1
-- exception

----------------------------------------

x = undefined
y = x `seq` "blah"

main = do
  print (snd (x, y))
