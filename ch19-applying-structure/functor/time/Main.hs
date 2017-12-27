{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.String
import Data.Time.Clock
import Web.Scotty

main :: IO ()
main = scotty 3000 $
  get "/time" $ do
    ctime <- liftIO $ fmap (fromString . show) getCurrentTime
    otime <- liftIO $ fmap (fromString . show) (offsetCurrentTime 1)
    html $ mconcat [ "<h3>   UTC time:  ", ctime, "</h3>"
                   , "<h3>Offset time:  ", otime, "</h3>" ]

offsetCurrentTime :: NominalDiffTime -> IO UTCTime
offsetCurrentTime offset =
  addUTCTime (offset * 24 * 3600) <$> getCurrentTime
