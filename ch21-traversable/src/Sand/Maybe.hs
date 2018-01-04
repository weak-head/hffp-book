module Sand.Maybe where

import Sand.ClassDef

data Maybe_ a =
    Nothing_
  | Just_ a
  deriving (Eq, Show, Ord)

maybe_ :: b -> (a -> b) -> Maybe_ a -> b
maybe_ d _ Nothing_  = d
maybe_ d f (Just_ a) = f a

instance (Monoid_ a) => Monoid_ (Maybe_ a) where
  mempty_ = Just_ mempty_
  mappend_ Nothing_ m = m
  mappend_ m Nothing_ = m
  mappend_ (Just_ a) (Just_ b) = Just_ (a `mappend_` b)

instance Functor_ Maybe_ where
  fmap_ f = maybe_ Nothing_ (Just_ . f)

instance Applicative_ Maybe_ where
  pure_ = Just_
  (<*>!) Nothing_  _ = Nothing_
  (<*>!) (Just_ f) v = fmap_ f v

instance Monad_ Maybe_ where
  return_ = pure_
  (>>=!)  = flip (maybe_ Nothing_)

instance Foldable_ Maybe_ where
  foldr_ f d = maybe_ d (`f` d)

instance Traversable_ Maybe_ where
  sequenceA_ Nothing_  = pure_ Nothing_
  sequenceA_ (Just_ a) = fmap_ Just_ a
