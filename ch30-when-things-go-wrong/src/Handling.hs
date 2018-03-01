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
