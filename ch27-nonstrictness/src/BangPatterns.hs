{-# LANGUAGE BangPatterns #-}

module BangPatterns where

-- > :set -dsuppress-all
-- > :set -ddump-simpl
-- > :l src/BangPatterns.hs

doesntEval :: Bool -> Int
doesntEval b = 1
-- doesntEval
-- doesntEval =
--   \ _ -> I# 1#

manualSeq :: Bool -> Int
manualSeq b = b `seq` 1
-- manualSeq
-- manualSeq =
--   \ b_a1ia ->
--     case b_a1ia of _
--       { __DEFAULT -> I# 1# }

banging :: Bool -> Int
banging !b = 1
-- banging
-- banging =
--   \ b_a1ie ->
--     case b_a1ie of _
--       { __DEFAULT -> I# 1# }

----------------------------------------

data Foo = Foo Int !Int

first (Foo x _) = x
second (Foo _ y) = y

dc1 = second (Foo undefined 1)
-- 1

dc2 = first (Foo 1 undefined)
-- exception

----------------------------------------

data DoesntForce =
  TisLazy Int String

gibString :: DoesntForce -> String
gibString (TisLazy _ s) = s


data BangBang =
  SheShotMeDown !Int !String

gimmeString :: BangBang -> String
gimmeString (SheShotMeDown _ s) = s


fd1 = let x = TisLazy undefined "blah"
      in gibString x
-- "blah"

fd2 = let s = SheShotMeDown
          x = s undefined "blah"
      in gimmeString x
-- exception
