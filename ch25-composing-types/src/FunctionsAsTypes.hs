module FunctionsAsTypes where

--
-- Identity :: * -> *
--
-- id :: a -> a
--
newtype Identity a =
  Identity { runIdentity :: a }

--
-- Compose :: (* -> *) -> (* -> *) -> * -> *
--
--     (.) :: (b -> c) -> (a -> b) -> a -> c
--
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)
-- > Compose [Just 1, Nothing]
-- Compose {getCompose = [Just 1, Nothing]}
--
-- > let xs = [Just (1::Int), Nothing]
-- > :t Compose xs
-- Compose [Just (1::Int), Nothing] :: Compose [] Maybe Int


instance Functor Identity where
  fmap f (Identity a) = Identity (f a)


instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose m) = Compose $ (fmap . fmap) f m

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure a = Compose $ pure $ pure a

  -- (<*>) :: f (a -> b) -> (f a -> f b)
  --
  -- Compose f g (a -> b) -> Compose f g a -> Compose f g b
  --
  --    f :: f (g (a -> b))
  --    m :: f (g     a)
  --
  --  fmap (<*>) f => f (g a -> g b)
  --             m :: f (g a)
  (Compose f) <*> (Compose m) = Compose $ fmap (<*>) f <*> m

-- Monad for Compose is not possible ^_^

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose m) = foldMap (foldMap f) m

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse = undefined

----------------------------------------------------------------------
-- One less bit of structure
newtype One f a =
  One (f a)
  deriving (Eq, Show)

instance Functor f => Functor (One f) where
  fmap f (One m) = One $ fmap f m

-- One more layer of structure
newtype Three f g h a =
  Three (f (g (h a)))
  deriving (Eq, Show)

instance (Functor f, Functor g, Functor h) => Functor (Three f g h) where
  fmap f (Three m) = Three $ (fmap . fmap . fmap) f m

v :: Compose [] Maybe (Compose Maybe [] Integer)
v = Compose [Just (Compose $ Just [1])]
