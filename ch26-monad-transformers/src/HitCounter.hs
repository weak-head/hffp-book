{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           System.Environment (getArgs)
import           Web.Scotty.Trans

------------------------------

data Config =
  Config
  { counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty  = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

------------------------------

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m = let mp = M.alter put k m
                in (mp, mp M.! k)
  where put Nothing  = Just 1
        put (Just n) = Just $ n + 1

-- ScottyT Text (ReaderT Config IO)
app :: Scotty ()
app =

  -- ActionT Text (ReaderT Config IO)
  get "/:key" $ do
    unprefixed <- param "key"
    pref       <- lift $ ReaderT $ \r -> return $ prefix r

    let key' = mappend pref unprefixed

    newInteger <- lift $ ReaderT $ \r -> do
      m <- readIORef $ counts r
      let (m', cnt) = bumpBoomp key' m
      writeIORef (counts r) m'
      return cnt

    html $ mconcat
      [ "<h1>Success! Count was: "
      , TL.pack $ show newInteger
      , "</h1>"
      ]

-- curl localhost:3000/someString
main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR r = runReaderT r config
  scottyT 3000 runR app
