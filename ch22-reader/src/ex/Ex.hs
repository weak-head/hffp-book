module Ex where

import Control.Monad
import Control.Applicative (liftA3)

data Record =
  Record { rTitle :: String
         , rLength :: Int
         , rTrusted :: Bool }
  deriving (Show, Eq)

readTitle :: String -> String
readTitle = id

readLength :: Foldable t => t a -> Int
readLength = length

readSigned :: String -> Bool
readSigned = (signature == ) . take (length signature)
  where signature = "[signed]"

readRecord :: String -> Record
readRecord = liftA3 Record readTitle readLength readSigned

run = mapM_ (print . readRecord) entries
  where
    entries =
      [ "Record <a>"
      , "[signed] Record <b>"
      , "[undefined] Recored <c>"
      ]
