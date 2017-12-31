module HttpC where

import Data.Monoid
import Data.Functor.Constant
import Data.Functor.Identity
import Data.ByteString.Lazy hiding (map)
import Network.Wreq

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traverseUrls :: IO [Response ByteString]
traverseUrls = traverse get urls

--- Traversable as Functor and Foldable ------------------------------

---------
-- Functor: fmap

fid = traverse (Identity . (+1)) [1, 2]
-- Identity [2, 3]

fid' = runIdentity fid
-- [2, 3]

fmap' :: Traversable t => (a -> b) -> t a -> t b
fmap' f t = runIdentity $ traverse (Identity . f) t

rem = fmap' (+1) [1..5]

----------
-- Foldable: foldMap

xs :: [Sum Integer]
xs = [1, 2, 3, 4, 5]

tcon = traverse (Constant . (+1)) xs
-- Constant (Sum {getSum = 20})

foldMap' :: (Traversable t, Monoid b) => (a -> b) -> t a -> b
foldMap' f t = getConstant $ traverse (Constant . f) t
