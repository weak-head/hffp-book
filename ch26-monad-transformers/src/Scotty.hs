{-# LANGUAGE OverloadedStrings #-}

{-

newtype ScottyT e m a =
  ScottyT { runS :: State (ScottyState e m) a }
  deriving (Functor, Applicative, Monad)

newtype ActionT e m a =
  ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
  deriving ( Functor, Applicative )

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO

-}

module Scotty where

import Web.Scotty
import Web.Scotty.Internal.Types (ActionT(..))
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class (lift)

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
--    lift $ putStrLn "hello"
    ActionT . lift . lift . lift $ putStrLn "hello"
    html $ mconcat [ "<h1>Scotty, "
                   , beam
                   , " me up!</h1>"]

{-
lift :: (Monad m, MonadTrans t) => m a -> t m a
lift :: (MonadTrans t) => IO a -> t IO a
lift :: IO a -> ActionM a
lift :: IO () -> ActionM ()

-- three lifts, one each for ExceptT, ReaderT and StateT.
instance MonadTrans (ActionT e) where
  lift = ActionT . lift . lift . lift

-}
