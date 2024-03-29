{-# LANGUAGE OverloadedStrings #-}

module Morra where

import qualified Control.Exception as Ex
import           Control.Monad.IO.Class ( liftIO )
import           Control.Monad.Loops ( iterateWhile )
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Bits ( xor )
import           Data.Bool ( bool )
import           Data.Maybe ( fromJust )
import           Data.Text.Lazy hiding (concat)
import           System.Random ( randomRIO, randomIO )

----------------------------------------------------------------------

-- | The possible game types:
--   * Player vs Player
--   * Player vs AI
--   * AI vs AI
data GameType = PvP | PvE | EvE deriving (Show, Eq)

-- | Human or IO.
data PlayerKind = PL | AI deriving (Show)

-- | The player name.
newtype PlayerName = PlayerName { getPlayerName :: Text }
  deriving (Show)

-- | The game configuration.
data GameConfig =
  GameConfig { bestOf     :: Int
             , gameKind   :: GameType
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
getAIChoise = randomRIO (1,2)

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

-- | Print choises and score.
printChoicesAndScore :: Int -> Int -> Bool -> GameState -> IO ()
printChoicesAndScore p1v p2v p1win s = do
  let p1n = unpack $ getPlayerName $ p1Name s
      p2n = unpack $ getPlayerName $ p2Name s
  liftIO $ putStrLn $ concat
    [ p1n, " has chosen ", show p1v, "; "
    , p2n, " has chosen ", show p2v, "; "
    , bool p2n p1n p1win, "'s victory; "
    , "New score: "
    , show $ p1Score s , "-" , show $ p2Score s ]

-- | Runs one round of the Mora game.
oneRound :: Round
oneRound = StateT $ \s -> do
  p1v <- getPlayerChoise (p1Name s) (p1Kind s)
  p2v <- getPlayerChoise (p2Name s) (p2Kind s)
  let (p1p, p2p) = evalScorePoints p1v p2v (fe s)
      s' = s { p1Score = p1Score s + p1p
             , p2Score = p2Score s + p2p }
  liftIO $ printChoicesAndScore p1v p2v (p1p == 1) s'
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

-- | Outputs the even player.
printEvenPlayer :: GameConfig -> IO ()
printEvenPlayer conf = do
  let pl = bool (p2N conf) (p1N conf) (firstEven conf)
  putStrLn $ concat [ unpack $ getPlayerName pl
                    , " has been selected as Even player." ]

-- | Runs the game based on the provided config.
runGame :: Game -> GameConfig -> IO GameResult
runGame game conf = do
  liftIO $ printEvenPlayer conf
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

----------------------------------------------------------------------

-- | Query user for the game kind.
selectGameType :: IO GameType
selectGameType = do
  putStrLn "Select the game type:"
  putStrLn "  1 - Player vs Player"
  putStrLn "  2 - Player vs AI"
  putStrLn "  3 - AI vs AI"
  Ex.handle handler reader
  where

    reader = do
      putStr "Your choice: "
      v <- readLn
      if v < 1 && v > 3
        then fail "out of range"
        else return $ toGameType v

    handler :: Ex.IOException -> IO GameType
    handler _ = do
      putStrLn "Failed to parse the input. Please input '1', '2' or '3'."
      selectGameType

    toGameType 1 = PvP
    toGameType 2 = PvE
    toGameType 3 = EvE


-- | Constructs the game configuration.
buildGameConfig :: IO GameConfig
buildGameConfig = do
  fstEv <- randomIO
  gt    <- selectGameType
  return GameConfig  { bestOf    = 3
                     , gameKind  = gt
                     , p1N       = PlayerName "John"
                     , p2N       = PlayerName "AI"
                     , firstEven = fstEv }

-- | Outputs the final game result.
printGameResult :: GameResult -> IO ()
printGameResult gr =
  putStrLn $ concat [ "------> "
                    , unpack . getPlayerName . fromJust . won $ gr
                    , " has won the game! Final score: "
                    , show $ p1S gr, "-", show $ p2S gr
                    , " <------" ]

main :: IO ()
main =
  buildGameConfig >>=
  runGame theGame >>=
  printGameResult
