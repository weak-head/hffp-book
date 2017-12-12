module EitherValidation where

-- f ~ Either e

type E = Either
-- (<*>) ::   f (a -> b) ->   f a ->   f b
-- (<*>) :: E e (a -> b) -> E e a -> E e b

-- pure :: a -> f a
-- pure :: a -> E e a

-- > pure 1 :: Either e Int
-- Right 1
--
-- > Right (+1) <*> Right 1
-- Right 2
--
-- > Right (+1) <*> Left "sad"
-- Left "sad"
--
-- > Left "sad" <*> Right "1"
-- Left "sad"
--
-- > Left "sad" <*> Left "sadface.png"
-- Left "sad"
