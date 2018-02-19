module ThunkLife where


myList = [1, 2] :: [Integer]
-- > let myList = [1,2] :: [Integer]
-- > :sprint myList
-- myList = [1, 2]

myList2 = [1, 2, 3]
-- > let myList2 = [1,2,3]
-- > :t myList2
-- myList2 :: Num t => [t]
-- > :sprint myList2
-- myList2 = _

xs = [1, 2, id 1, 4, undefined, 6] :: [Integer]
-- > let xs = [1, 2, id 1, 4, undefined, 6] :: [Integer]
-- > :sprint xs
-- xs = [1, 2, _, 4, _, 6]

xs2  = [1, 2, id 1] :: [Integer]
xs2' = xs ++ undefined
-- > let xs2  = [1, 2, id 1] :: [Integer]
-- > let xs2' = xs ++ undefined
-- > :sprint xs2'
-- xs2' = _
