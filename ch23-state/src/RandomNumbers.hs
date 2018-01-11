module RandomNumbers where

import System.Random

-- mkStdGen :: Int -> StdGen
-- next :: StdGen g => g -> (Int, g)
-- random  :: (RandomGen g, Random a) => g -> (a, g)
-- randomR :: (RandomGen g, Random a) => (a, a) -> g (a, g)

mkSG = mkStdGen 0
nSG  = next mkSG

newSg  = snd (next mkSG)
newSg' = next newSg
newSgg = next (snd (next newSg))
