{-# LANGUAGE OverloadedStrings #-}

module Morra where

import qualified Control.Exception as Ex
import           Control.Monad.Loops ( iterateWhile )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits ( xor )
import           Data.Bool ( bool )
import           Data.Text.Lazy hiding (concat)
import           System.Random ( randomRIO )

----------------------------------------------------------------------

-- | The possible game types:
--   * Player vs Player
--   * Player vs AI
--   * AI vs AI
data GameKind = PvP | PvE | EvE deriving (Show, Eq)

-- | Human or IO.
data PlayerKind = PL | AI deriving (Show)

-- | The player name.
newtype PlayerName = PlayerName { getPlayerName :: Text }
  deriving (Show)

-- | The game configuration.
data GameConfig =
  GameConfig { bestOf     :: Int
             , gameKind   :: GameKind
             , p1N        :: PlayerName
             , p2N        :: PlayerName
             , firstEven  :: Bool }

-- | The game state.
data GameState =
  GameState { p1Score :: Int
            , p2Score :: Int
            , p1Kind  :: PlayerKind
            , p2Kind  :: PlayerKind
            , p1Name  :: PlayerName
            , p2Name  :: PlayerName
            , fe      :: Bool }

-- | The game result.
data GameResult =
  GameResult { p1S :: Int
             , p2S :: Int
             , won :: Maybe PlayerName }
  deriving ( Show )

type Round = StateT GameState IO GameResult
type Game  = ReaderT GameConfig (StateT GameState IO) GameResult

----------------------------------------------------------------------

-- | Evaluates score poins for the players, depending
-- on which player is even or odd.
evalScorePoints :: Int -> Int -> Bool -> (Int, Int)
evalScorePoints a b p =
  let c = a + b
      e = even c
      r = e `xor` p
  in (bti $ not r, bti r)
  where bti = bool 0 1

-- | Query user for the choise.
queryUser :: PlayerName -> IO Int
queryUser n = do
  putStr $ concat [ "Your choise, "
                  , unpack $ getPlayerName n
                  , ": " ]
  Ex.catch reader handler
  where
    handler :: Ex.IOException -> IO Int
    handler _ = do
      putStrLn "Failed to parse the input. Please input '1' or '2'."
      queryUser n

    reader = do
      v <- readLn
      if v == 1 || v == 2
        then return v
        else fail "out of range"


-- | Tries to predict user behavior and gets
-- the possible winning value.
getAIChoise :: IO Int
getAIChoise = randomRIO (0,1)

-- | Gets player choise. Depending on player kind
-- either prompt a user or generate a value.
getPlayerChoise :: PlayerName -> PlayerKind -> IO Int
getPlayerChoise n kind = case kind of
  PL -> queryUser n
  AI -> getAIChoise

-- | Converts 'GameState' to 'GameResult'.
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

-- | Runs one round of the Mora game.
oneRound :: Round
oneRound = StateT $ \s -> do
  p1v <- getPlayerChoise (p1Name s) (p1Kind s)
  p2v <- getPlayerChoise (p2Name s) (p2Kind s)
  let (p1p, p2p) = evalScorePoints p1v p2v (fe s)
      s' = s { p1Score = p1Score s + p1p
             , p2Score = p2Score s + p2p }
  return (toGameResult s', s')

-- | Creates the Morra game.
theGame :: Game
theGame = ReaderT $ \conf ->
  iterateWhile (isRunning conf) oneRound
  where
    isRunning :: GameConfig -> GameResult -> Bool
    isRunning c r = let s = bestOf c `div` 2
                    in not $ (p1S r > s) || (p2S r > s)

----------------------------------------------------------------------

-- | Runs the game based on the provided config.
runGame :: GameConfig -> Game -> IO GameResult
runGame conf game =
  fst <$> runStateT (runReaderT game conf) initialState
  where
    initialState = GameState
      { p1Score = 0
      , p2Score = 0
      , p1Kind  = bool PL AI (gameKind conf == EvE)
      , p2Kind  = bool AI PL (gameKind conf == PvP)
      , p1Name  = p1N conf
      , p2Name  = p2N conf
      , fe      = firstEven conf }

main = do
  let conf  = GameConfig 3 PvE (PlayerName "John") (PlayerName "AI") True
  gameResult <- runGame conf theGame
  print gameResult
