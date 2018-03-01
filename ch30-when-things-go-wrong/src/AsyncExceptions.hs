module AsyncExceptions where

import Control.Concurrent ( forkIO , threadDelay )
import Control.Exception
import System.IO

----------------------------------------

data ExitThread =
  ExitThread
  deriving (Eq, Show)

instance Exception ExitThread

------------------------------------------

openAndWrite :: IO ()
openAndWrite = do
  h <- openFile "data.txt" WriteMode
  threadDelay 1500
  hPutStr h $ replicate 50000000 '0' ++ "xd"
  hClose h

main :: IO ()
main = do
  threadId <- forkIO $ mask_ openAndWrite
  threadDelay 1540
  putStrLn "here"
  throwTo threadId ExitThread
  putStrLn "there"
  threadDelay 1000

----------------------------------------
