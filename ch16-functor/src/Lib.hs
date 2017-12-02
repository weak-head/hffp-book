{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Lib
    ( someFunc
    , fmapApp
    , fmapFM
    ) where

someFunc :: IO ()
someFunc = print fmapApp


-- | Functor definition.
--
-- 'f' has kind * -> *
--
--
class Functor f where
-- has kind:  *    ->  *  ->  *
  fmap :: (a -> b) -> f a -> f b


-- we cannot specify @(Either Int String) because fmap alters the type
-- in the container and that last type cannot be fixed
fmapApp = Prelude.fmap @(Either Int) (++ " test") (Right "value" )

newtype FM b c a = FM a
instance Prelude.Functor (FM a b) where
  fmap f (FM c) = FM $ f c

fmapFM = Prelude.fmap @(FM Int String)


---

class Sn a where
  s :: a -> a
--     * -> *

-- {-# LANGUAGE MultiParamTypeClasses #-}
class Ee where
  e :: b -> f (g a b c)
-- b :: *
-- a :: *
-- c :: *
-- f :: * -> *
-- g :: * -> * -> * -> *

-- Bifunctor
class By where
  sr :: e a b
     -> (a -> c)
     -> (b -> d)
     -> e c d
-- a :: *
-- c :: *
-- b :: *
-- d :: *
-- e :: * -> * -> *


--class Im v where
--  impossibleKind :: v -> v a
--
--class Im2 v where
--  impossibleKind2 :: v a -> v


-- a :: *
f1 :: a -> a
f1 = undefined

-- a :: *
-- b :: * -> *
-- t :: * -> *
f2 :: a -> b a -> t (b a)
f2 = undefined

-- a :: *
-- b :: *
-- c :: * -> * -> *
f3 :: c a b -> c b a
f3 = undefined
