module Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) =
    Moi $ \s -> let (a, s') = g s
                in (f a, s')

moiFunc = runMoi ((+1) <$> (Moi $ \s -> (0, s))) 0

---

instance Applicative (Moi s) where
  pure x = Moi $ (,) x
  (Moi g) <*> (Moi f) =
    Moi $ \s -> let (a,  s') = f s
                    (fa, sr) = g s'
                in (fa a, sr)

---

instance Monad (Moi s) where
  return = pure
  (Moi g) >>= f =
    Moi $ \s -> let (a, s') = g s
                    (Moi k) = f a
                in k s
