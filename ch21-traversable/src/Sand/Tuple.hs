module Sand.Tuple where

import Sand.ClassDef

data Tuple a b =
  Tuple a b
  deriving (Show, Eq)

instance (Monoid_ a, Monoid_ b) => Monoid_ (Tuple a b) where
  mempty_ = Tuple mempty_ mempty_
  mappend_ (Tuple a b) (Tuple x v) =
    Tuple (a `mappend_` x) (b `mappend_` v)

instance Functor_ (Tuple a) where
  fmap_ f (Tuple a b) = Tuple a (f b)

instance Monoid_ a => Applicative_ (Tuple a) where
  pure_ = Tuple mempty_
  (Tuple a f) <*>! (Tuple b v) =
    Tuple (a `mappend_` b) (f v)

instance Monoid_ a => Monad_ (Tuple a) where
  return_ = pure_
  (Tuple a v) >>=! f =
    let (Tuple b v') = f v
    in Tuple (a `mappend_` b) v'

instance Foldable_ (Tuple a) where
  foldr_ f d (Tuple _ b) = f b d

instance Traversable_ (Tuple a) where
  traverse_ f (Tuple a b) = fmap_ (Tuple a) (f b)
