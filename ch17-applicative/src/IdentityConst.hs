module IdentityConst where

import Data.Functor.Identity
import Data.Functor.Const
import Data.Monoid

--------------------------------------------------
--- Identity

-- f ~ Identity
-- Applicative f =>
type Id = Identity

-- (<*>) ::  f (a -> b) -> f a -> f b
-- (<*>) :: Id (a -> b) -> f a -> f b

-- pure :: a ->  f a
-- pure :: a -> Id a


vo = [1, 2, 3]
va = [9, 9, 9]

cst = const <$> vo <*> va
--  = [1,1,1,2,2,2,3,3,3]

mkId = Identity

cst' = const <$> mkId vo <*> mkId va
--   = Identity [1, 2, 3]

newtype OIdentity a = OIdentity a
  deriving (Eq, Ord, Show)

instance Functor OIdentity where
  fmap f (OIdentity a) = OIdentity (f a)

instance Applicative OIdentity where
  pure = OIdentity
  (<*>) (OIdentity f) (OIdentity v) = OIdentity (f v)


--------------------------------------------------
--- Const

-- f ~ Constant e
type C = Const

-- (<*>) ::   f (a -> b) ->   f a ->   f b
-- (<*>) :: C e (a -> b) -> C e a -> C e b

-- pure :: a -> f a
-- pure :: a -> C e a


f = Const (Sum 1)
g = Const (Sum 2)

fApG = f <*> g
-- Const (Sum {getSum = 3})

wut = Const undefined <*> g
-- Const (Sum {getSum = *** Exception: Prelude.undefined})

pu = pure 1 :: Const String Int
-- Const ""

newtype OCons a b =
  OCons { getOConst :: a }
  deriving (Eq, Ord, Show)

instance Functor (OCons a) where
  fmap _ (OCons a) = OCons a

instance Monoid a => Applicative (OCons a) where
  pure _ = OCons mempty
  (<*>) (OCons a') (OCons a) = OCons (mappend a' a)
