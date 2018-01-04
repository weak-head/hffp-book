module Sand.Either where

import Sand.ClassDef

data Either_ a b =
    Left_ a
  | Right_ b
  deriving (Show, Eq)

either_ :: (a -> c) -> (b -> c) -> Either_ a b -> c
either_ f _ (Left_ a)  = f a
either_ _ g (Right_ a) = g a

instance Functor_ (Either_ a) where
  fmap_ f = either_ Left_ (Right_ . f)

instance Applicative_ (Either_ a) where
  pure_ = Right_
  (Left_ e)  <*>! _ = Left_ e
  (Right_ f) <*>! v = fmap_ f v

instance Monad_ (Either_ a) where
  return_ = pure_
  m >>=! f = either_ Left_ f m

instance Foldable_ (Either_ a) where
  foldr_ f d = either_ (const d) (`f` d)

instance Traversable_ (Either_ a) where
  traverse_ f = either_ (pure_ . Left_) (fmap_ Right_ . f)
