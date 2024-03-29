{-# LANGUAGE ExistentialQuantification #-}

module Hierarchy where

import Data.Typeable
import Control.Exception

----------------------------------------

data SomeBaseException = forall e . Exception e => SomeBaseException e

instance Show SomeBaseException where
  show (SomeBaseException e) = show e

instance Exception SomeBaseException

baseExceptionToException :: Exception e => e -> SomeException
baseExceptionToException = toException . SomeBaseException

baseExceptionFromException :: Exception e => SomeException -> Maybe e
baseExceptionFromException x = do
  SomeBaseException e <- fromException x
  cast e

----------------------------------------

data SomeSpecificException = forall e . Exception e => SomeSpecificException e

instance Show SomeSpecificException where
  show (SomeSpecificException e) = show e

instance Exception SomeSpecificException where
  toException   = baseExceptionToException
  fromException = baseExceptionFromException

specificExceptionToException :: Exception e => e -> SomeException
specificExceptionToException = toException . SomeSpecificException

specificExceptionFromException :: Exception e => SomeException -> Maybe e
specificExceptionFromException x = do
  SomeSpecificException e <- fromException x
  cast e

----------------------------------------

data ConcreteException = ConcreteException
  deriving (Show)

instance Exception ConcreteException where
  toException   = specificExceptionToException
  fromException = specificExceptionFromException

----------------------------------------

main :: IO ()
main = do
  let throwEx = throw ConcreteException
  --
  putStrLn "Catching exception with [Concrete handler]"
  catch throwEx concreteHandler
  putStrLn ""
  --
  putStrLn "Catching exception with [Specific handler]"
  catch throwEx specificHandler
  putStrLn ""
  --
  putStrLn "Catching exception with [Base handler]"
  catch throwEx baseHandler
  putStrLn ""
  --
  putStrLn "Catching exception with [Some handler]"
  catch throwEx someHandler
  putStrLn ""
  --
  putStrLn "Catching exception with [All -> some first]"
  catches throwEx [ Handler ioHandler
                  , Handler someHandler
                  , Handler baseHandler
                  , Handler specificHandler
                  , Handler concreteHandler ]
  putStrLn ""
  --
  putStrLn "Catching exception with [All -> concrete first]"
  catches throwEx [ Handler ioHandler
                  , Handler concreteHandler
                  , Handler specificHandler
                  , Handler baseHandler
                  , Handler someHandler
                  ]
  putStrLn ""
  --
  putStrLn "Catching exception with [IO handler]"
  catch throwEx ioHandler
  putStrLn ""

  where
    concreteHandler :: ConcreteException -> IO ()
    concreteHandler e = putStrLn $ "> Caught (concr): " ++ show e
    --
    specificHandler :: SomeSpecificException -> IO ()
    specificHandler e = putStrLn $ "> Caught (specific): " ++ show e
    --
    baseHandler :: SomeBaseException -> IO ()
    baseHandler e = putStrLn $ "> Caught (base): " ++ show e
    --
    someHandler :: SomeException -> IO ()
    someHandler e = putStrLn $ "> Caught (some): " ++ show e
    --
    ioHandler :: IOException -> IO ()
    ioHandler e = putStrLn $ "> Caught (io): " ++ show e

{-

Catching exception with [Concrete handler]
> Caught (concr): ConcreteException

Catching exception with [Specific handler]
> Caught (specific): ConcreteException

Catching exception with [Base handler]
> Caught (base): ConcreteException

Catching exception with [Some handler]
> Caught (some): ConcreteException

Catching exception with [All -> some first]
> Caught (some): ConcreteException

Catching exception with [All -> concrete first]
> Caught (concr): ConcreteException

Catching exception with [IO handler]
*** Exception: ConcreteException

-}
