module Sand.Identity where

import Sand.ClassDef

newtype Identity a =
  Identity { unId :: a }
  deriving (Eq, Show)

instance Monoid_ a => Monoid_ (Identity a) where
  mempty_ = Identity mempty_
  mappend_ (Identity a) (Identity b) = Identity (a `mappend_` b)

instance Functor_ Identity where
  fmap_ f (Identity a) = Identity (f a)

instance Applicative_ Identity where
  pure_ = Identity
  (Identity f) <*>! (Identity v) = Identity (f v)

instance Monad_ Identity where
  return_ = pure_
  (Identity a) >>=! f = f a

instance Foldable_ Identity where
  foldr_ f d (Identity a) = f a d

instance Traversable_ Identity where
  traverse_ f (Identity a) = fmap_ Identity (f a)
