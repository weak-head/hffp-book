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


ex1 :: IO ()
ex1 = mapM_ exec [1..12]
  where
    exec f = catchBoth (evenAndThreeDiv f) >>= print

----------------------------------------
