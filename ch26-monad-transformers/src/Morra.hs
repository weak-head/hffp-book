module Morra where

import Data.Text.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Loops ( iterateWhile )

----------------------------------------------------------------------

newtype Player = Player Text

data GameConfig =
  GameConfig { bestOf :: Int }

data GameState =
  GameState { player1Score :: Int
            , player2Score :: Int }

data GameResult =
  GameResult { finished :: Bool
             , won :: Maybe Player }

type Game = ReaderT GameConfig (StateT GameState IO) GameResult

----------------------------------------------------------------------

oneRound :: Game
oneRound = undefined

-- | Play the Morra, best of N.
makeGame :: Game
makeGame = iterateWhile finished oneRound

----------------------------------------------------------------------

runGame :: GameConfig -> Game -> IO GameResult
runGame conf game =
  fst <$> runStateT (runReaderT game conf) initialState
  where
    initialState = GameState 0 0

main = do
  let conf  = GameConfig 3
  gameResult <- runGame conf makeGame
  putStrLn "tbd"
