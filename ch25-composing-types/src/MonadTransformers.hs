module MonadTransformers where

--- Monadic stacking -------------------------------------------------

f1 = fmap (+1) (Just 1)

f2 = (,,) <$> Just 1 <*> Just "h" <*> Just [1, 2]

-- IO (Reader String [a])


--- IdentityT --------------------------------------------------------

newtype Identity a =
  Identity { runIdentity :: a }
  deriving (Eq, Show)

newtype IdentityT f a =
  IdentityT { runIdentityT :: f a }
  deriving (Eq, Show)


instance Functor Identity where
  fmap f (Identity a) = Identity $ f a

instance Functor m => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT $ fmap f fa


instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity v) = Identity $ f v

instance Applicative m => Applicative (IdentityT m) where
  pure x = IdentityT $ pure x
  (IdentityT f) <*> (IdentityT v) = IdentityT $ f <*> v


instance Monad Identity where
  return = pure
  (Identity v) >>= f = f v

instance Monad m => Monad (IdentityT m) where
  return = pure
  (IdentityT v) >>= f = IdentityT $ v >>= (runIdentityT . f)
