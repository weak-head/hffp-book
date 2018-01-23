{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module LogFile where

import Data.Map
import Data.Time
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import Text.Trifecta

theLogExample :: ByteString
theLogExample = [r|
-- a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
13:00 Programming
17:00 Commuting home in rover
17:30 R&R
19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

# 2025-02-07 -- dates not nececessarily sequential
08:00 Breakfast -- should I try skippin bfast?
09:00 Bumped head, passed out
13:36 Wake up, hadache
13:37 Go to medbay
13:40 Patch self up
13:45 Commute home for rest
14:15 Read
21:00 Dinner
21:15 Read
22:00 Sleep

|]


-- > fromGregorianValid 2008 10 22
-- Just 2008-10-22
-- > show $ fromJust $ fromGregorianValid 2008 10 22
-- "2008-10-22" :: String
-- https://two-wrongs.com/haskell-time-library-tutorial

type Time         = UTCTime
type Activity     = String

newtype Log =
  Log { log :: Map Day DailyActivityLog }
  deriving (Eq, Show)

newtype DailyActivityLog =
  DailyActivityLog { dayLog :: Map Time Activity }
  deriving (Eq, Show)
