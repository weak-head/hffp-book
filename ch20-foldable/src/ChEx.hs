module ChEx where


newtype Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldr f b (Constant a) = f a b

--------------------------------------------------

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldr f v (Two _ b) = f b v

--------------------------------------------------

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldr f v (Three a b c) = f c v

--------------------------------------------------

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldr f v (Three' a b c) = f c v

--------------------------------------------------

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldr f v (Four' a b c d) = f d v

--------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a))
        => (a -> Bool) -> t a -> f a
filterF f = foldMap mget
  where mget a | f a       = pure a
               | otherwise = mempty
