{-# LANGUAGE OverloadedStrings #-}

module Scot where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Text.Lazy
import System.Environment ( getArgs )
import Web.Scotty.Trans

----------------------------------------------------------------------

data Config =
  Config { arguments :: Text }

----------------------------------------------------------------------

act :: ActionT Text (ReaderT Config IO) ()
act = do
  v <- lift $ ReaderT $ return . arguments
  html $ mconcat
    [ "<h1>App args: "
    , v
    , "</h1>" ]

----------------------------------------------------------------------

scot :: ScottyT Text (ReaderT Config IO) ()
scot = get "/:key" act

----------------------------------------------------------------------

main :: IO ()
main = do
  [arg] <- getArgs
  let conf   = Config $ pack arg
      runR r = runReaderT r conf
  scottyT 3000 runR scot
