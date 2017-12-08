module ListApplicativeSpec where

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  -- a -> f a
  pure x = Cons x Nil
  -- f (a -> b) -> f a -> f b
  -- > (<*>) fs xs = [f x | f <- fs, x <- xs]
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  --(<*>) (Cons f fs) xs = conc (f <$> xs) (fs <*> xs)
  --(<*>) fs xs = fold (\v -> conc (fmap v xs)) Nil fs
  (<*>) fs xs = flatMap ($xs) (fmap fmap fs)

-- mapping single value over multiple functions
-- > map ($x) fs
-- > map ($4) [(+2), (*3)]
--
-- mapping a bunch of values over bunch of functions
-- > concatMap ($xs) fs
--
--
-- > f = (^2)
-- > g = (+10)
--
-- concatMap ($ [1,2,3,4,10]) [(f <$>), (g <$>) . tail . reverse]
--
-- ~
--
-- concat $ [(f <$>), (g <$>) . tail . reverse] <*> [[1,2,3,4,10]]
--

conc :: List a -> List a -> List a
conc Nil v = v
conc (Cons v xs) ys = Cons v (conc xs ys)


f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
ap = f <*> v
-- Cons 2 (Cons 3 (Cons 2 (Cons 4 Nil)))

---- -- --

fold :: (a -> b -> b) -> b -> List a -> b
fold _ v Nil = v
fold f v (Cons x xs) = f x (fold f v xs)

flat :: List (List a) -> List a
flat = fold conc Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f = flat . fmap f
