module Mread where

newtype Mread r a =
  Mread { doRead :: r -> a }

instance Functor (Mread r) where
  fmap f (Mread g) = Mread $ f . g

instance Applicative (Mread r) where
  pure x = Mread $ const x
  (Mread f) <*> (Mread g) =
    Mread $ \r -> let r' = g r
                  in f r r'

instance Monad (Mread r) where
  return = pure
  (Mread g) >>= f =
    Mread $ \r -> let r' = g r
                  in doRead (f r') r

----------------------------------------------------------------------

{-

--------------
-- Functor: --

 f = a -> b

      |------|
 rd = |  r   | -> a
      |------|

-- f <$> rd  =>
                f   b
      |------|   \ /
 rd = |  r   | -> a
      |------|

 ~=~

      |------|
 rd = |  r   | -> b
      |------|


------------------
-- Applicative: --

          |------|
 f =      |  r   | -> a -> b
          |------|

      |------|
 rd = |  r   | -> a
      |------|

-- f <*> rd   =>

  ___
 | r | -----------------
  ---                  |
   |                  \ /
   |                |------|
   |           rd = |  r   | -> a
   |                |------|    |
   |                            |
   |--------------------        |
                       |        |
                      \ /       |
                    |------|   \ /
           f =      |  r   | -> a -> b
                    |------|

~=~

  |------|
  |  r   | -> b
  |------|



------------
-- Monad: --

Same as applicative, but with flipped arguments for 'f'.

-}
