module CoreDump where


-- :set -dsuppress-all
-- :set -ddump-simpl
-- :l src/CoreDump.hs

----

discriminatory :: Bool -> Int
discriminatory b =
  case b of
    False -> 0
    True  -> 1

----

discriminatory' :: Bool -> Int
discriminatory' b =
  let x = undefined
  in case b of
    False -> 0
    True  -> 1

----

discriminatory'' :: Bool -> Int
discriminatory'' b =
  let x = undefined
  in case x `seq` b of
    False -> 0
    True  -> 1
