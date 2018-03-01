module Bottoms where

import Control.Exception

worksGood :: IO (Either SomeException ())
worksGood = try undefined

cantCatch :: IO (Either SomeException ())
cantCatch = try $ return undefined

main :: IO ()
main = do
  worksGood >>= print
  putStrLn "--"
  cantCatch >>= print
  return ()
