--{-# LANGUAGE FlexibleInstances #-}

module Sand.Func where

import Sand.ClassDef

newtype Func a b =
  Func { unFunc :: (a -> b) }

newtype FuncM a =
  FuncM { unFuncM :: (a -> a) }

---

instance Monoid_ b => Monoid_ (Func a b) where
  mempty_ = Func $ const mempty_
  mappend_ (Func f) (Func g) =
    Func $ \a -> let v1 = f a
                     v2 = g a
                 in v1 `mappend_` v2

instance Monoid_ a => Monoid_ (FuncM a) where
  mempty_ = FuncM $ id
  mappend_ (FuncM f) (FuncM g) = FuncM $ (f . g)

---

instance Functor_ (Func a) where
  fmap_ f (Func g) = Func $ f . g

instance Applicative_ (Func a) where
  pure_ x = Func $ const x
  (Func f) <*>! (Func g) = Func $ \r -> f r (g r)

instance Monad_ (Func a) where
  return_ = pure_
  (Func g) >>=! f =
    Func $ \r -> unFunc (f (g r)) r
