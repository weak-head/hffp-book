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
