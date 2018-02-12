{-# LANGUAGE OverloadedStrings #-}
module InUse where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           Web.Scotty

----------------------------------------------------------------------

param' :: Parsable a => Text -> ActionM (Maybe a)
param' k = rescue (Just <$> param k)
                  (const (return Nothing))

param'' :: Parsable a => Text -> MaybeT ActionM a
param'' k = MaybeT $ rescue (Just <$> param k)
                            (const (return Nothing))

parame' :: Parsable a => Text -> ActionM (Either String a)
parame' k =
  rescue (Right <$> param k)
         (const $ return $ Left $
           "The key: " ++ show k ++ " was missing!")

parame'' :: Parsable a => Text -> ExceptT String ActionM a
parame'' k = ExceptT $
  rescue (Right <$> param k)
         (const $ return $ Left $
           "The key: " ++ show k ++ " was missing!")

tshow :: Show a => a -> Text
tshow = TL.pack . show

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

-- > curl -s "http://localhost:3000/ego/wrd"
  get "/ego/:word" $ do
    beam <- param "word"
    a <- parame' "1"
    let a' = either (const 0) id a
    liftIO $ print (a :: Either String Int)
    liftIO $ print (a' :: Int)
    html $ mconcat [ "<h1>Scotty, "
                   , beam
                   , " me up!</h1>" ]

-- > curl -s "http://localhost:3000/nego?1=2&2=3&3=4&4=5"
  get "/nego" $ do
    reco <- runExceptT $ do
      a <- parame'' "1"
      liftIO $ print a
      b <- parame'' "2"
      c <- parame'' "3"
      d <- parame'' "4"
      (lift . lift) $ print b
      return ((a, b, c, d) :: Reco)
    case reco of
      (Left e) -> text (TL.pack e)
      (Right r) ->
        html $ mconcat [ "<h1>Success! Reco was: "
                       , tshow r
                       , "</h1>"]
      
