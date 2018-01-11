module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

data Die =
    DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie 1 = DieOne
intToDie 2 = DieTwo
intToDie 3 = DieThree
intToDie 4 = DieFour
intToDie 5 = DieFive
intToDie 6 = DieSix
-- It's better to use Maybe insted of the 'error' here
intToDie n = error $ "only 1-6, got: " ++ show n

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
  let s = mkStdGen 0
      (d1, s1) = randomR (1, 6) s
      (d2, s2) = randomR (1, 6) s1
      (d3, _)  = randomR (1, 6) s2
  (intToDie d1, intToDie d2, intToDie d3)

----

rollDie :: State StdGen Die
rollDie = state $ do
  (n, s) <- randomR (1, 6)
  return (intToDie n, s)

-- state :: Monad m => (s -> (a, s)) -> StateT s m a

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1, 6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

es :: (Die, Die, Die)
es = evalState rollDieThreeTimes' (mkStdGen 0)

-- replicateM :: Monad m => Int -> m a -> m [a]

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie'

evalnDie :: [Die]
evalnDie = evalState (nDie 5) (mkStdGen 0)

rollsToGetN :: Int -> StdGen -> (Int, [Die])
rollsToGetN = go 0 (0, [])
  where
    go :: Int -> (Int, [Die]) -> Int -> StdGen -> (Int, [Die])
    go sum (count, h) msum gen
      | sum >= msum = (count, h)
      | otherwise =
          let (die, nextGen) = randomR (1, 6) gen
          in go (sum + die) (count + 1, intToDie die : h) msum nextGen

rro :: (Int, [Die])
rro = rollsToGetN 20 (mkStdGen 0)

-- randomIO :: Random a => IO a

rrIO :: IO (Int, [Die])
rrIO = (rollsToGetN 20 . mkStdGen) <$> randomIO
