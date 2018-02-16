{-# LANGUAGE OverloadedStrings #-}

module Morra where

import Control.Monad.Trans.Class
import Data.Text.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Loops ( iterateWhile )

----------------------------------------------------------------------

-- | There are there possible game types:
--   * Player vs Player
--   * Player vs AI
--   * AI vs AI
data GameKind = PvP | PvE | EvE deriving (Show)

-- | A player could be an actula human or AI.
data PlayerKind = PL | AI deriving (Show)

-- | The player name.
newtype Player = Player Text
  deriving (Show)

-- | The game configuration.
data GameConfig =
  GameConfig { bestOf :: Int
             , gameKind :: GameKind
             , p1Name :: Player
             , p2Name :: Player }

-- | The game state.
data GameState =
  GameState { player1Score :: Int
            , player2Score :: Int
            , player1Kind :: PlayerKind
            , player2Kind :: PlayerKind }

-- | The game result.
data GameResult =
  GameResult { p1S :: Int
             , p2S :: Int
             , won :: Maybe Player }
  deriving ( Show )

type Round = StateT GameState IO GameResult
type Game  = ReaderT GameConfig (StateT GameState IO) GameResult

----------------------------------------------------------------------

oneRound :: Round
oneRound = StateT $ \s ->
   return (GameResult 2 1 (Just $ Player "Name"), s)

-- | Play the Morra.
theGame :: Game
theGame = ReaderT $ \conf ->
  iterateWhile (isRunning conf) oneRound
  where
    isRunning :: GameConfig -> GameResult -> Bool
    isRunning c r = let s = bestOf c `div` 2
                    in not $ (p1S r > s) || (p2S r > s)

----------------------------------------------------------------------

runGame :: GameConfig -> Game -> IO GameResult
runGame conf game =
  fst <$> runStateT (runReaderT game conf) initialState
  where
    initialState = GameState 0 0 PL AI

main = do
  let conf  = GameConfig 3 PvE (Player "John") (Player "AI")
  gameResult <- runGame conf theGame
  print gameResult
