module Queue where

import Data.Maybe (fromJust)
import Criterion.Main

----------------------------------------

newtype QueuePlain a =
  QueuePlain { queuePlain :: [a] }
  deriving (Show, Eq)

pushPlain :: a -> QueuePlain a -> QueuePlain a
pushPlain x xs = QueuePlain $ x : queuePlain xs

popPlain :: QueuePlain a -> Maybe (a, QueuePlain a)
popPlain q = case queuePlain q of
  [] -> Nothing
  xs -> Just (last xs, QueuePlain $ init xs)

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

push' :: a -> Queue a -> Queue a
push' a (Queue en de) =
  Queue { enqueue = a : en
        , dequeue = de }

pop' :: Queue a -> Maybe (a, Queue a)
pop' (Queue en de) =
  case de of
    []     -> maybePop en
    (x:xs) -> Just (x, Queue en xs)
  where
    maybePop [] = Nothing
    maybePop xs = pop' $ Queue [] $ reverse xs

----------------------------------------

sequantialQueuePlain :: Int -> QueuePlain Int
sequantialQueuePlain n =
  let que = QueuePlain []
      lng = foldr pushPlain que [1..n]
  in unwrap lng
  where
    unwrap q = case popPlain q of
      Just (_, nq) -> unwrap nq
      Nothing      -> QueuePlain []

sequantialQueueV1 :: Int -> Queue Int
sequantialQueueV1 n =
  let que = Queue [] []
      lng = foldr push que [1..n]
  in unwrap lng
  where
    unwrap q = case pop q of
      Just (_, nq) -> unwrap nq
      Nothing      -> Queue [] []

sequantialQueueV2 :: Int -> Queue Int
sequantialQueueV2 n =
  let que = Queue [] []
      lng = foldr push' que [1..n]
  in unwrap lng
  where
    unwrap q = case pop' q of
      Just (_, nq) -> unwrap nq
      Nothing      -> Queue [] []

----

alteringQueuePlain :: Int -> QueuePlain Int
alteringQueuePlain n =
  foldr pushPop (QueuePlain []) [1..n]
  where pushPop a q =
          let q' = pushPlain a q
          in snd $ fromJust $ popPlain q'

alteringQueueV1 :: Int -> Queue Int
alteringQueueV1 n =
  foldr pushPop (Queue [] []) [1..n]
  where pushPop a q =
          let q' = push a q
          in snd $ fromJust $ pop q'

alteringQueueV2 :: Int -> Queue Int
alteringQueueV2 n =
  foldr pushPop (Queue [] []) [1..n]
  where pushPop a q =
          let q' = push' a q
          in snd $ fromJust $ pop' q'

----

alteringSeriesPlain :: Int -> QueuePlain Int
alteringSeriesPlain n =
  foldr pushPop (QueuePlain []) [1..n]
  where pushPop a q =
          let q' = pushPlainN 5 a q
          in unwrap q'
        pushPlainN n a q = foldr pushPlain q (replicate n a)
        unwrap q = case popPlain q of
          Just (_, nq) -> unwrap nq
          Nothing      -> QueuePlain []

alteringSeriesV1 :: Int -> Queue Int
alteringSeriesV1 n =
  foldr pushPop (Queue [] []) [1..n]
  where pushPop a q =
          let q' = pushPlainN 5 a q
          in unwrap q'
        pushPlainN n a q = foldr push q (replicate n a)
        unwrap q = case pop q of
          Just (_, nq) -> unwrap nq
          Nothing      -> Queue [] []

alteringSeriesV2 :: Int -> Queue Int
alteringSeriesV2 n =
  foldr pushPop (Queue [] []) [1..n]
  where pushPop a q =
          let q' = pushPlainN 5 a q
          in unwrap q'
        pushPlainN n a q = foldr push' q (replicate n a)
        unwrap q = case pop' q of
          Just (_, nq) -> unwrap nq
          Nothing      -> Queue [] []

----------------------------------------

-- > stack build --ghc-options -O2
-- > stack exec bl-queue -- --output bench.html
runBench :: IO ()
runBench  = defaultMain
  [ bench "QueuePlain sequential test" $ whnf sequantialQueuePlain 9999
  , bench "QueueV1    sequential test" $ whnf sequantialQueueV1 9999
  , bench "QueueV2    sequential test" $ whnf sequantialQueueV2 9999
  ---
  , bench "QueuePlain altering test" $ whnf alteringQueuePlain 9999
  , bench "QueueV1    altering test" $ whnf alteringQueueV1 9999
  , bench "QueueV2    altering test" $ whnf alteringQueueV2 9999
  ---
  , bench "QueuePlain series test" $ whnf alteringSeriesPlain 2555
  , bench "QueueV1    series test" $ whnf alteringSeriesV1 2555
  , bench "QueueV2    series test" $ whnf alteringSeriesV2 2555
  ]
