module ChEx where

import Control.Monad.Trans.Reader
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
