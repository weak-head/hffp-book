module WhatHappens where

import System.IO.Unsafe
import Control.Concurrent

myData :: IO (MVar Int)
myData = newEmptyMVar

main :: IO ()
main = do
  mv <- myData
  putMVar mv 0
  mv' <- myData
  zero <- takeMVar mv'
  print zero

----------------------------------------

{-# NOINLINE myData' #-}
myData' :: MVar Int
myData' = unsafePerformIO newEmptyMVar

main' :: IO ()
main' = do
  putMVar myData' 0
  zero <- takeMVar myData'
  print zero
