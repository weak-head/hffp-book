module FoldableIntro where

import Data.Foldable
import Data.Monoid

-- foldable is a way of generalizing
-- catamorphisms - folding - to different datatypes

-- class Foldable t where
--     {-# MINIMAL foldMap | foldr #-}
--   fold    :: Monoid m => t m -> m
--   foldMap :: Monoid m => (a -> m) -> t a -> m

----------

f  = foldr (+) 0 [1..5] -- sum [1..5]

fs = fold $ Sum <$> [1..5]
fp = fold $ Product <$> [1..5]

fl = fold ["h", "j"]

fms = foldMap Sum     [1..5]
fmp = foldMap Product [1..5]
fmd = foldMap All     [True, False, True]
fmc = foldMap Any     [True, False, True]
fmf = foldMap First   [Nothing, Just 1, Nothing, Just 5]
fml = foldMap Last    [Nothing, Just 1, Just 5, Nothing]

----------

fmpa = foldMap (*5) $ Product <$> [1..3]
fmsa = foldMap (*5) $ Sum     <$> [1..3]

fmons = foldr (*) 3 $ Sum     <$> [2..4] -- 72
fmonp = foldr (*) 3 $ Product <$> [2..4] -- 72

fjp  = foldMap (*5) (Just 100) :: Product Integer -- 500
fjs  = foldMap (*5) (Just 5)   :: Sum Integer -- 25
fjpn = foldMap (*5) (Nothing)  :: Product Integer -- 1
fjsn = foldMap (*5) (Nothing)  :: Sum Integer -- 0
