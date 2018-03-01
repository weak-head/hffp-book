module StopTheHop where

import Control.Concurrent ( threadDelay )
import Control.Exception
import Control.Monad ( forever )
import System.Random ( randomRIO )

getNext :: IO ()
getNext = do
  nxt <- randomRIO (1, 10 :: Int)
  if nxt `elem` [1..9]
    then throwIO DivideByZero
    else throwIO StackOverflow

main :: IO ()
main = forever $ do
  let tryS :: IO () -> IO (Either ArithException ())
      tryS = try
  v <- tryS getNext
  putStrLn $ "next: " ++ show v
  threadDelay 1000000           -- 1s
