module Main where

import Shady.Db

main :: IO ()
main = do
  initDb
  putStrLn someFunc
