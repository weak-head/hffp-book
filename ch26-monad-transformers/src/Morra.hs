{-# LANGUAGE OverloadedStrings #-}

module Morra where

import Control.Monad.Trans.Class
import Data.Text.Lazy
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Loops ( iterateWhile )

----------------------------------------------------------------------

newtype Player = Player Text
  deriving ( Show )

data GameConfig =
  GameConfig { bestOf :: Int }

data GameState =
  GameState { player1Score :: Int
            , player2Score :: Int }

data GameResult =
  GameResult { running :: Bool
             , won :: Maybe Player }
  deriving ( Show )

type Round = StateT GameState IO GameResult
type Game = ReaderT GameConfig (StateT GameState IO) GameResult

----------------------------------------------------------------------

-- oneRound :: Game
-- oneRound =
--   ReaderT $ \conf ->
--     StateT $ \s ->
--                 return (GameResult False (Just $ Player "Name"), s)

makeRound :: Round
makeRound = StateT $ \s ->
                return (GameResult False (Just $ Player "Name"), s)

-- | Play the Morra, best of N.
makeGame :: Game
makeGame = ReaderT $ \conf ->
  iterateWhile running makeRound

----------------------------------------------------------------------

runGame :: GameConfig -> Game -> IO GameResult
runGame conf game =
  fst <$> runStateT (runReaderT game conf) initialState
  where
    initialState = GameState 0 0

main = do
  let conf  = GameConfig 3
  gameResult <- runGame conf makeGame
  print gameResult
