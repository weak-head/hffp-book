module Handling where

import Control.Exception
import Data.Typeable

----------------------------------------

handler :: SomeException -> IO ()
handler (SomeException e) = do
  print $ typeOf e
  putStrLn $ "Error: " ++ show e

ex1 :: IO ()
ex1 =
  writeFile "ff.html" "<h1>Hey</h1>" `catch` handler

----------------------------------------

willIFail :: Integer -> IO (Either ArithException ())
willIFail denom = try $ print $ div 5 denom

ex2 :: IO ()
ex2 = willIFail 0 >>= print
   >> willIFail 1 >>= print

----------------------------------------

onlyReportError :: Show e => IO (Either e a) -> IO ()
onlyReportError action = do
  result <- action
  case result of
    Left e -> print e
    Right _ -> return ()

willFail' :: Integer -> IO ()
willFail' denom = onlyReportError $ willIFail denom

ex3 :: IO ()
ex3 = willFail' 0 >> willFail' 1

----------------------------------------

willFail'' :: Integer -> IO ()
willFail'' denom =
  print (div 5 denom) `catch` handler
  where handler :: ArithException -> IO ()
        handler = print

----------------------------------------

runFail :: IO ()
runFail =
  mapM_ willFail'' [0, 1, 2, 10, -1]
