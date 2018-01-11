module FizzBuzzState where

import           Control.Monad
import           Control.Monad.Trans.State
import qualified Data.DList as DL


fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5  == 0 = "Buzz"
           | n `mod` 3  == 0 = "Fizz"
           | otherwise       = show n

fizzBuzzMain = mapM_ (putStrLn . fizzBuzz) [1..100]

--- Using State ------------------------------------------------------

fizzbuzzList :: [Integer] -> [String]
fizzbuzzList list = execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main = mapM_ putStrLn $ reverse $ fizzbuzzList [1..100]

--- Using DList ------------------------------------------------------

fizzbuzzList2 :: [Integer] -> DL.DList String
fizzbuzzList2 list = execState (mapM_ addResult2 list) DL.empty

addResult2 :: Integer -> State (DL.DList String) ()
addResult2 n = do
  xs <- get
  let result = fizzBuzz n
  put (DL.snoc xs result)

main2 :: IO ()
main2 = mapM_ putStrLn $ fizzbuzzList2 [1..100]

--- The other way ----------------------------------------------------

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo b a = fizzbuzzList [a, a + (signum $ b - a)..b]

main3 :: IO ()
main3 = mapM_ putStrLn $ fizzbuzzFromTo 1 100
