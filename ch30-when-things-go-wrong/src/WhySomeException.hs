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

main :: IO ()
main = do
  print $ multiError 0
  print $ multiError 1
  print $ multiError 2
