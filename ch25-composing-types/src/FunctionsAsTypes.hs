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
  traverse f (Compose m) = Compose <$> traverse (traverse f) m

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

----------------------------------------------------------------------

class Bifunctor p where
  {-# MINIMAL bimap | first, second #-}

  bimap  :: (a -> b) -> (c -> d) -> p a c -> p b d
  bimap f g = first f . second g

  first  :: (a -> b) -> p a c -> p b c
  first f = bimap f id

  second :: (b -> c) -> p a b -> p a c
  second = bimap id

data Deux a b = Deux a b
instance Bifunctor Deux where
  bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a
instance Bifunctor Const where
  bimap f _ (Const a) = Const (f a)

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
  bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
  bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d
instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)

instance Bifunctor Either where
  bimap f _ (Left a)  = Left $ f a
  bimap _ g (Right b) = Right $ g b
