module WHNF where

dc = (,) undefined undefined

noDc = undefined

lam = \_ -> undefined


-- Up to data contstructor or lambda
r1 = dc `seq` 1                 -- 1
r2 = noDc `seq` 1               -- exception
r3 = lam `seq` 1                -- 1

----------------------------------------------------------------------

data Test =
    A Test2
  | B Test2
  deriving (Show)

data Test2 =
    C Int
  | D Int
  deriving (Show)

forceNothing :: Test -> Int
forceNothing _ = 0

forceTest :: Test -> Int
forceTest (A _) = 1
forceText (B _) = 2

forceTest2 :: Test -> Int
forceTest2 (A (C i)) = i
forceTest2 (B (C i)) = i
forceTest2 (A (D i)) = i
forceTest2 (B (D i)) = i

fn1 = forceNothing undefined     -- 0
fn2 = forceNothing (A undefined) -- 0

ft1 = forceTest (A undefined)   -- 1
ft2 = forceTest (B undefined)   -- 2
ft3 = forceTest undefined       -- exception

fs1 = forceTest2 (A (C 0))         -- 0
fs2 = forceTest2 (A (C undefined)) -- exception
fs3 = forceTest2 (A undefined)     -- exception
fs4 = forceTest2 undefined         -- exception
