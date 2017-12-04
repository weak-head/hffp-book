{-# LANGUAGE FlexibleInstances #-}

module ChExercises where

import GHC.Arr

--------------------------------------------------

data Boo a =
    F a
  | T a

instance Functor Boo where
  fmap f (F a) = F (f a)
  fmap f (T a) = T (f a)

--------------------------------------------------

data MBoo a =
    MF
  | MT a

instance Functor MBoo where
  fmap _ MF = MF
  fmap f (MT a) = MT (f a)

--------------------------------------------------

--  f :: * -> *
-- Mu :: (* -> *) -> *
newtype Mu f = InF { outF :: f (Mu f)}

mkMuf :: Mu Maybe
mkMuf = InF $ Just $ InF $ Just $ InF Nothing

-- instance Functor Mu where
--   fmap f m = undefined

--------------------------------------------------

-- Array :: (* -> * -> *)
--     D :: *
data D =
  D (Array Word Word) Int Int


--------------------------------------------------

data Sum b a =
    First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a)  = First (f a)
  fmap _ (Second b) = Second b

--------------------------------------------------

data Company a c b =
    DeepBlue a c
  | Something b

instance Functor (Company a b) where
  fmap f (DeepBlue a c) = DeepBlue a c
  fmap f (Something b) = Something (f b)

--------------------------------------------------

data More a b =
    L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More a) where
  fmap f (L a b c) = L a (f b) c
  fmap f (R a b c) = R (f a) b (f c)

--------------------------------------------------

data Quant a b =
    Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)

--------------------------------------------------

newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

--------------------------------------------------

newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))

--------------------------------------------------

newtype EvilGoateeConst a b =
  GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

--------------------------------------------------

newtype LiftItOut f a =
  LiftItOut (f a)

instance (Functor f) => Functor (LiftItOut f) where
  fmap f (LiftItOut m) = LiftItOut $ fmap f m

--------------------------------------------------

data Parappa f g a =
  DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa m n) = DaWrappa (fmap f m) (fmap f n)

--------------------------------------------------

data IgnoreOne f g a b =
  IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething m n) = IgnoringSomething m (fmap f n)

--------------------------------------------------

data Notorious g o a t =
  Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious m k n) = Notorious m k (fmap f n)

--------------------------------------------------

data List a =
    Nil
  | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

--------------------------------------------------

data GoatLord a =
    NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
              (GoatLord a)
              (GoatLord a)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

--------------------------------------------------

data TalkToMe a =
    Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt        = Halt
  fmap f (Print s a) = Print s (f a)
  fmap f (Read g)    = Read (f . g)
