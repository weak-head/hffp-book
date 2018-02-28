{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}

module WhySomeException where

import Control.Exception ( ArithException (..)
                         , AsyncException (..) )
import Data.Typeable

{-

data SomeException where
  SomeException :: Exception e => e -> SomeException

~=

data SomeException =
  forall e . Exception e => SomeException e

-}

data MyException =
  forall e . (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left $ MyException DivideByZero
    1 -> Left $ MyException StackOverflow
    _ -> Right n

----------------------------------------

data SomeError =
    Arith ArithException
  | Async AsyncException
  | SomethingElse
  deriving (Show)

discriminateError :: MyException -> SomeError
discriminateError (MyException e) =
  case cast e of
    (Just arith) -> Arith arith
    Nothing ->
      case cast e of
        (Just async) -> Async async
        Nothing -> SomethingElse

runDisc n =
  either discriminateError (const SomethingElse) (multiError n)

doRunDisc = do
  print $ runDisc 0
  print $ runDisc 1
  print $ runDisc 2

----------------------------------------

main :: IO ()
main = do
  print $ multiError 0
  print $ multiError 1
  print $ multiError 2
