module Moi where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Test.QuickCheck
import Test.QuickCheck.Gen
import Control.Applicative

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

---

instance Show (Moi a b) where
  show _ = "Moi a b"

instance (Arbitrary s, CoArbitrary s, Arbitrary a) => Arbitrary (Moi s a) where
  arbitrary = promoteMoi (\s -> coarbitrary s arbitrary)
                         (\s -> coarbitrary s arbitrary)

promoteMoi :: (s -> Gen a) -> (s -> Gen s) -> Gen (Moi s a)
promoteMoi f g =
  MkGen (\q r -> Moi $ \s -> let MkGen sg = g s
                                 MkGen ag = f s
                             in (ag q r, sg q r))

instance (Arbitrary s, Eq s, Eq a, EqProp s, EqProp a, Show a, Show s) => EqProp (Moi s a) where
  (Moi f) =-= (Moi g) = property (liftA2 (=-=) f g)

batchMoi = do
  quickBatch (functor (undefined :: Moi Int (String, Int, Bool)))
  quickBatch (applicative (undefined :: Moi Int (String, Int, Bool)))
  quickBatch (monad (undefined :: Moi Int (String, Int, Bool)))  -- woooop! monad is broken :D


{-- Visualizing State instances --------------------------------------

--------------
-- Functor: --

 f = (a -> b)

      |----------|          |---------|
 st = |    s     | -> ( a , |    s'   | )
      |----------|          |---------|


-- f <$> st =>
                      f   b
      |----------|     \ /  |---------|
 st = |    s     | -> ( a , |    s'   | )
      |----------|          |---------|

~=~

      |----------|          |---------|
 st = |    s     | -> ( b , |    s'   | )
      |----------|          |---------|



------------------
-- Applicative: --

      |----------|                 |---------|
 f  = |    s     | -> ( (a -> b) , |    s'   | )
      |----------|                 |---------|

      |----------|          |---------|
 st = |    s     | -> ( a , |    s'   | )
      |----------|          |---------|

-- f <*> st =>

      |----------|          |---------|
 st = |    s     | -> ( a , |    s'   | )
      |----------|          |---------|
                       /
                     /           |
                   /             |
                 /              \ /
               /                 -
             /
           /                |----------|                 |---------|
          |            f  = |    s     | -> ( (a -> b) , |    s'   | )
          |                 |----------|                 |---------|
          |
~=~      \ /                               |__________________________|
          _                               /
                    |---------|       <--/
       ( (a -> b) , |    s'   | )
                    |---------|

---------------------------------------------------------------------}
