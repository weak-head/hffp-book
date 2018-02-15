module Morra where

import Control.Monad.Trans.State

----------------------------------------------------------------------

data GameState =
  GameState { player1Score :: Int
            , player2Score :: Int }

type Mor = StateT GameState IO

----------------------------------------------------------------------

main = do
  putStrLn "tbd"
