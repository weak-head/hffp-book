module Sand.Constant where

import Sand.ClassDef

newtype Constant a b =
  Constant { unConst :: a }
  deriving (Eq, Show)

instance Monoid_ a => Monoid_ (Constant a b) where
  mempty_ = Constant mempty_
  mappend_ (Constant a) (Constant b) =
    Constant $ a `mappend_` b

instance Functor_ (Constant a) where
  fmap_ _ (Constant a) = Constant a

instance Monoid_ a => Applicative_ (Constant a) where
  pure_ _ = Constant mempty_
  (Constant a) <*>! (Constant b) =
    Constant $ a `mappend_` b

instance Monoid_ a => Monad_ (Constant a) where
  return_ = pure_
  (Constant a) >>=! _ = Constant a

instance Foldable_ (Constant a) where
  foldr_ _ d _ = d

instance Traversable_ (Constant a) where
  traverse_ _ (Constant a) = pure_ $ Constant a
