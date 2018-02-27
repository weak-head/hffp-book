module Queue where

import Criterion.Main

----------------------------------------

data Queue a =
  Queue { enqueue :: [a]
        , dequeue :: [a]
        } deriving (Show, Eq)

----------------------------------------

push :: a -> Queue a -> Queue a
push a (Queue en de) =
  Queue { enqueue = a : en
        , dequeue = de ++ [a] }

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue en de) =
  case de of
    []     -> Nothing
    (x:xs) -> Just (x, Queue { enqueue = init en
                             , dequeue = xs })

----------------------------------------

sequantialQueue1 :: Int -> Queue Int
sequantialQueue1 n =
  let que = Queue [] []
      lng = foldr push que [1..n]
  in unwrap lng
  where
    unwrap q = case pop q of
      Just (_, nq) -> unwrap nq
      Nothing      -> Queue [] []

-- > stack build --ghc-options -O2
-- > stack exec bl-queue -- --output bench.html
runBench :: IO ()
runBench  = defaultMain
  [ bench "Queue#1 sequential test" $ whnf sequantialQueue1 9999 ]
