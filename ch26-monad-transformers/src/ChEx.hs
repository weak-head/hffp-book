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

