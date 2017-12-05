module MonoidalFunctor where

import Control.Applicative
import Data.Monoid

-- applicative functors are monoidal functors

-- ($)   ::    (a -> b) ->   a   -> b
-- (<$>) ::    (a -> b) -> f a -> f b
-- (<*>) ::  f (a -> b) -> f a -> f b

-- ($)   function application
-- (<$>) fmap
-- (<*>) ap


-- :: f (a -> b) -> f a -> f b
--
--    f              f      f
--    (a -> b)       a      b
--
-- similar to
--
--    mappend :: Monoid a => a -> a -> a
--
-- mappend :: f             f      f
-- ($)     ::   (a -> b)      a      b
--
-- (<*>)   :: f (a -> b) -> f a -> f b


mf1 = [(*2), (*3)] <*> [4, 5]
--  = [2 * 4, 2 * 5, 3 * 4, 3 * 5]
--  = [8, 10, 12, 15]

mf2 = Just (*2) <*> Just 2
--  = Just 4

mf3 = Just (*2) <*> Nothing
--  = Nothing

mf4 = Nothing <*> Just 2
--  = Nothing

mf5 = Nothing <*> Nothing
--  = Nothing


-- :info (,)
-- data (,) a b = (,) a b
--
-- instance Monoid a => Applicative ((,) a)
--
-- instance (Monoid a, Monoid b) => Monoid (a, b)

mf6 = ("Woo", (+1)) <*> (" Hoo!", 0)
--  = ("Woo Hoo!", 1)

mf7 = (Sum 2, (+1)) <*> (Sum 0, 0)
--  = (Sum {getSum = 2},1)

mf8 = (Product 3, (+9)) <*> (Product 2, 8)
--  = (Product {getProduct = 6}, 17)

mf9 = (All True, (+1)) <*> (All False, 0)
--  = (All {getAll = False}, 1)


-- instance (Monoid a, Monoid b) => Monoid (a, b) where
--   mempty = (mempty, mempty)
--   mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)

-- instance Monoid a => Applicative ((,) a) where
--   pure x = (mempty, x)
--   (<*>) (u,f) (v,a) = (mappend u v, f a)




-- instance Monoid a => Monoid (Maybe a) where
--   mempty = Nothing
--   mappend m Nothing = m
--   mappend Nothing m = m
--   mappend (Just a) (Just b) = Just (mappend a b)

-- instance Applicative Maybe where
--   pure = Just
--   (<*>) Nothing _ = Nothing
--   (<*>) _ Nothing = Nothing
--   (<*>) (Just f) (Just v) = Just (f v)
