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
             , p1N :: Player
             , p2N :: Player }

-- | The game state.
data GameState =
  GameState { p1Score :: Int
            , p2Score :: Int
            , p1Kind :: PlayerKind
            , p2Kind :: PlayerKind
            , p1Name :: Player
            , p2Name :: Player }

-- | The game result.
data GameResult =
  GameResult { p1S :: Int
             , p2S :: Int
             , won :: Maybe Player }
  deriving ( Show )

type Round = StateT GameState IO GameResult
type Game  = ReaderT GameConfig (StateT GameState IO) GameResult

----------------------------------------------------------------------

evalScorePoints :: Int -> Int -> (Int, Int)
evalScorePoints _ _ = (1, 0)

getPlayerChoise :: PlayerKind -> IO Int
getPlayerChoise _ = return 1

toGameResult :: GameState -> GameResult
toGameResult s =
  GameResult { p1S = p1Score s
             , p2S = p2Score s
             , won = winner
             }
  where
    winner = case compare (p1Score s) (p2Score s) of
      EQ -> Nothing
      LT -> Just $ p2Name s
      GT -> Just $ p1Name s

oneRound :: Round
oneRound = StateT $ \s -> do
  p1v <- getPlayerChoise $ p1Kind s
  p2v <- getPlayerChoise $ p2Kind s
  let (p1p, p2p) = evalScorePoints p1v p2v
      s' = s { p1Score = p1Score s + p1p
             , p2Score = p2Score s + p2p }
  return (toGameResult s', s')

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
    initialState = GameState 0 0 PL AI (p1N conf) (p2N conf)

main = do
  let conf  = GameConfig 3 PvE (Player "John") (Player "AI")
  gameResult <- runGame conf theGame
  print gameResult
