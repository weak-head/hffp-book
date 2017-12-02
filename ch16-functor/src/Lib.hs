{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Lib
    ( someFunc
    , fmapApp
    , fmapFM
    , Fdd(..)
    , Fd(..)
    , brokenIdLaw
    , brokenCompositionLaw
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


---------------

-- Function application:
--   ($) :: (a -> b) -> a -> b

-- Function application "over"/"through" a 'container'.
-- lifts a function over abstract structure.
--  fmap :: (a -> b) -> f a -> f b
-- (<$>) :: (a -> b) -> f a -> f b


data Fd a =
    Em
  | Fd a
  | Cm
  deriving (Show, Eq)

instance Prelude.Functor Fd where
  fmap _ Em = Em
  fmap _ Cm = Cm
  fmap f (Fd a) = Fd $ f a


data Fdd a =
    Emm
  | Fdd a
  | Cmm
  deriving (Show, Eq, Prelude.Functor) -- DeriveFunctor

--- Breaking identity law...

data BadF a =
    Yep
  | Ahr
  | Moo a
  deriving (Show, Eq)

instance Prelude.Functor BadF where
  fmap _ Yep = Ahr
  fmap _ Ahr = Yep
  fmap f (Moo a) = Moo $ f a

brokenIdLaw = (Prelude.fmap id Yep) == (id @(BadF Int) Yep)

--- Another bad functor that breaks composition law...

data BadCount a =
  Heisenberg Int a
  deriving (Eq, Show)

instance Prelude.Functor BadCount where
  fmap f (Heisenberg n a) = Heisenberg (n+1) (f a)

brokenCompositionLaw =
  let f = (++ " some")
      g = (++ " value")
      h = Heisenberg 0 "yep"
  in (Prelude.fmap f . Prelude.fmap g) h == Prelude.fmap (f . g) h
