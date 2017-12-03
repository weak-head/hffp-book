{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module LibSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Function

main = hspec spec

spec = parallel $ do
  describe "Functor" $ do
    it "Identity law" $ do
      property @([Int] -> Bool) functorIdentity
    it "Composition law" $ do
      property @([Int] -> Fun Int Int -> Fun Int Int -> Bool) functorCompose

----------------
-- Functor laws:
--
-- fmap id = id
-- fmap (f . g) = (fmap f) . (fmap g)
---------------

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Functor f, Eq (f c)) =>
                  f a
               -> Fun a b
               -> Fun b c
               -> Bool
functorCompose x (Fun _ f) (Fun _ g)  =
  fmap (g . f) x == ((fmap g) . (fmap f)) x
