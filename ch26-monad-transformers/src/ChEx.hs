module ChEx where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

----------------------------------------------------------------------

rDec :: Num a => Reader a a
rDec = ReaderT $ \n -> Identity $ n - 1

rDec' :: Num a => Reader a a
rDec' = ReaderT $ Identity . flip (-) 1

r1 :: Integer
r1 = runReader rDec' 1
-- 0

r2 :: [Integer]
r2 = fmap (runReader rDec') [1..10]
-- [0..9]

----------------------------------------------------------------------

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \r -> Identity $ show r

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ Identity . show


s1 :: String
s1 = runReader rShow' 1
-- "1"

s2 :: [String]
s2 = fmap (runReader rShow') [1..10]
-- ["1".."10"]

----------------------------------------------------------------------

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc =
  ReaderT $ \n -> do
    print $ "Hi: " ++ show n
    return $ n + 1

p1 :: IO Integer
p1 = runReaderT rPrintAndInc 1
-- "Hi: 1"

p2 :: IO [Integer]
p2 = traverse (runReaderT rPrintAndInc) [1..10]
-- "Hi: 1-10"

----------------------------------------------------------------------

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum =
  StateT $ \s -> do
    print $ "Hi: " ++ show s
    return (show s, s + 1)

st1 = runStateT sPrintIncAccum 10
-- Hi: 10
-- ("10", 11)

st2 = mapM (runStateT sPrintIncAccum) [1..5]
-- Hi: 1
-- Hi: 2
-- Hi: 3
-- Hi: 4
-- Hi: 5
-- [("1", 2), ("2", 3), ("3", 4), ("4", 5), ("5", 6)]

----------------------------------------------------------------------
