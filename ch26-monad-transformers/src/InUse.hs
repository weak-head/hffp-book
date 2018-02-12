{-# LANGUAGE OverloadedStrings #-}
module InUse where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import Web.Scotty

----------------------------------------------------------------------

param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k)
                  (const (return Nothing))

param'' :: Parsable a => Text -> MaybeT ActionM a
param'' k = MaybeT $ rescue (Just <$> param k)
                            (const (return Nothing))

type Reco =
  (Integer, Integer, Integer, Integer)

----------------------------------------------------------------------

main = scotty 3000 $ do

-- > curl -s "http://localhost:3000/go/theWord?num=3777"
  get "/go/:word" $ do
    beam' <- param' "word"
    let beam = fromMaybe "" beam'
    i <- param' "num"
    liftIO $ print (i :: Maybe Integer)
    html $ mconcat [ "<h1>Scotty, "
                   , beam
                   , " me up!</h1>" ]

-- > curl -s "http://localhost:3000/ngo/someText?1=22&2=33&3=44&4=55"
  get "/ngo/:word" $ do
    beam <- param "word"
    reco <- runMaybeT $ do
      a <- param'' "1"
      liftIO $ print a
      b <- param'' "2"
      c <- param'' "3"
      d <- param'' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    liftIO $ print reco
    html $ mconcat [ "<h1>Scotty, "
                   , beam
                   , " me up!</h1>" ]
