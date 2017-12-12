module EitherValidation where

import Data.Monoid

-- f ~ Either e

type E = Either
-- (<*>) ::   f (a -> b) ->   f a ->   f b
-- (<*>) :: E e (a -> b) -> E e a -> E e b

-- pure :: a -> f a
-- pure :: a -> E e a

-- > pure 1 :: Either e Int
-- Right 1
--
-- > Right (+1) <*> Right 1
-- Right 2
--
-- > Right (+1) <*> Left "sad"
-- Left "sad"
--
-- > Left "sad" <*> Right "1"
-- Left "sad"
--
-- > Left "sad" <*> Left "sadface.png"
-- Left "sad"


data Validation err a =
    Failure err
  | Success a
  deriving (Eq, Show)

-- natural tranformations:

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a)   = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a)  = Success a

-- eitherToValid . validToEither == id
-- validToEither . eitherToValid == id

data Errors =
    DevidedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

success =   Success (+1)
        <*> Success 1 :: Validation String Int
-- success = Success 2

failure =   Success (+1)
        <*> Failure [StackOverflow]
-- failure = Failure [StackOverflow]

failure' =   Failure [StackOverflow]
         <*> Success (+1)
-- failure' = Failure [StackOverflow]

failures =
      Failure [MooglesChewedWires]
  <*> Failure [StackOverflow]
-- failures = Failure [MooglesChewedWires, StackOverflow]

instance Functor (Validation err) where
  fmap _ (Failure err) = Failure err
  fmap f (Success a)   = Success (f a)

instance Monoid err => Applicative (Validation err) where
  pure a = Success a
  (<*>) (Failure f) (Failure v) = Failure $ f <> v
  (<*>) (Failure f) _           = Failure f
  (<*>) _           (Failure v) = Failure v
  (<*>) (Success f) (Success v) = Success (f v)
