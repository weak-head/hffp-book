module MakingOwnExceptions where

import Control.Exception

----------------------------------------

data NotDivThree =
  NotDivThree Int
  deriving (Show, Eq)

data NotEven =
  NotEven Int
  deriving (Show, Eq)

instance Exception NotDivThree
instance Exception NotEven

----------------------------------------

evenAndThreeDiv :: Int -> IO Int
evenAndThreeDiv i
  | rem i 3 /= 0 = throwIO $ NotDivThree i
  | odd i        = throwIO $ NotEven i
  | otherwise    = return i


catchNotDivThree :: IO Int
                 -> (NotDivThree -> IO Int)
                 -> IO Int
catchNotDivThree = catch


catchNotEven :: IO Int
             -> (NotEven -> IO Int)
             -> IO Int
catchNotEven = catch


catchBoth :: IO Int -> IO Int
catchBoth ioInt =
  --catches :: IO a -> [Handler a] -> IO a
  catches ioInt
  [ Handler (\(NotEven n) -> return maxBound)
  , Handler (\(NotDivThree n) -> return minBound)
  ]
{-
data Handler a where
  Handler :: Exception e => (e -> IO a) -> Handler a
-}

ex1 :: IO ()
ex1 = mapM_ exec [1..12]
  where
    exec f = catchBoth (evenAndThreeDiv f) >>= print

----------------------------------------
----------------------------------------

data EATD =
    NotEven' Int
  | NotDivideThree' Int
  deriving (Eq, Show)

instance Exception EATD

evenAndDivThree' :: Int -> IO Int
evenAndDivThree' n
  | rem n 3 /= 0 = throwIO $ NotDivideThree' n
  | even n       = throwIO $ NotEven' n
  | otherwise    = return n

type EA e = IO (Either e Int)

ex2 :: IO ()
ex2 = mapM_ exec [1..12]
  where
    exec n = (try $ evenAndDivThree' n :: EA EATD) >>= print

