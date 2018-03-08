module Main where

import Shady.Server
import Shady.Db

main :: IO ()
main = do
  initDb
  startServer "" 4343
  putStrLn someFunc
