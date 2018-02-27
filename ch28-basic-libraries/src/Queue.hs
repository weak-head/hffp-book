module Queue where

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

----------------------------------------      

-- > stack build --ghc-options -O2
-- > stack exec bl-queue -- --output bench.html
runBench :: IO ()
runBench  = defaultMain
  [ bench "QueuePlain sequential test" $ whnf sequantialQueuePlain 9999
  , bench "QueueV1    sequential test" $ whnf sequantialQueueV1 9999
  ]
