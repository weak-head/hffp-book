module Sand.Maybe where

import Sand.ClassDef

data Maybe_ a =
    Nothing_
  | Just_ a
  deriving (Eq, Show, Ord)

instance (Monoid_ a) => Monoid_ (Maybe_ a) where
  mempty_ = Just_ mempty_
  mappend_ Nothing_ (Just_ a) = Just_ a
  mappend_ (Just_ a) Nothing_ = Just_ a
  mappend_ (Just_ a) (Just_ b) = Just_ (a `mappend_` b)
