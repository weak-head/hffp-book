module Reader where

import Control.Applicative

newtype Reader r a =
  Reader { runReader :: r -> a }

---

instance Functor (Reader a) where
  fmap f (Reader ra) = Reader $ f . ra

ask :: Reader a a
ask = Reader id

---

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)

data Person =
  Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog =
  Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "BB")
         (DogName "B")
         (Address "SS")

chris :: Person
chris =
  Person (HumanName "CA")
         (DogName "P")
         (Address "AA")

getDog :: Person -> Dog
getDog p = Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR = Dog <$> dogName <*> address

(<$->>) :: (a -> b) -> (r -> a) -> (r -> b)
(<$->>) = (<$>)

(<*->>) :: (r -> a -> b) -> (r -> a) -> (r -> b)
(<*->>) = (<*>)

getDogR' :: Person -> Dog
getDogR' = Dog <$->> dogName <*->> address


getDogR'' :: Person -> Dog
getDogR'' = liftA2 Dog dogName address


mLA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
mLA2 f ma mb = f <$> ma <*> mb


asks :: (r -> a) -> Reader r a
asks = Reader

instance Applicative (Reader a) where
  pure x = Reader $ const x
  (Reader g) <*> (Reader f) =
    Reader $ \a -> g a (f a)

---

foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

barOne :: Foldable t => t a -> (t a, Int)
barOne t = (t, length t)

barPlus :: Num a => [a] -> ([a], Int)
barPlus r = (foo r, length r)

frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
fooBind m k = \r -> k (m r) r
-- (>>=)
