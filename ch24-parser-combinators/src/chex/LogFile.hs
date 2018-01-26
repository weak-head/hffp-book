{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module LogFile where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.Map as DM
import           Data.Text hiding (empty)
import qualified Data.Time as DT
import           Text.Parser.LookAhead
import           Text.RawString.QQ
import           Text.Trifecta
--import Debug.Trace (traceShowM)

theLogExample :: ByteString
theLogExample = [r|
-- a comment

# 2025-02-05
08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
-- -_-
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
--
-- > parseTimeM True defaultTimeLocale "%R" "12:00" :: Maybe TimeOfDay

type Time         = DT.TimeOfDay
type Activity     = Text

newtype Log =
  Log { log :: DM.Map DT.Day [ActivityRecord] }
  deriving (Eq, Show)

data ActivityRecord =
  ActivityRecord { startTime :: Time
                 , activity  :: Activity
                 , endTime   :: Time
                 } deriving (Eq, Show)

skipComments :: Parser ()
skipComments =
  skipMany (string "--" >> manyTill anyChar (void newline <|> eof))

skipEmpty :: Parser ()
skipEmpty = skipMany (space <|> newline <|> tab)

skipEol :: Parser ()
skipEol = skipMany newline

-- | Parses and validates the activity start/end time.
parseTime :: Parser Time
parseTime = do
  hh <- fromIntegral <$> natural
  char ':'
  mm <- fromIntegral <$> natural
  case DT.makeTimeOfDayValid hh mm 0 of
     Nothing -> empty
     Just v  -> return v

-- | Parses and validates the day.
parseDay :: Parser DT.Day
parseDay = do
  year <- fromIntegral <$> natural
  char '-'
  month <- fromIntegral <$> natural
  char '-'
  day <- fromIntegral <$> natural
  case DT.fromGregorianValid year month day of
    Nothing -> empty
    Just v  -> return v

-- | Extracts the time when the activity has finished
-- (using the positive lookahead). In case if the time could
-- not be extracted the default value of 23:59:00 is being used.
lookAheadEndTime :: Parser Time
lookAheadEndTime =
  (lookAhead parseTime) <|> (return $ DT.TimeOfDay 23 59 00)

-- | Parses activity text, trims the whitespaces and skips comments.
parseActivity :: Parser Activity
parseActivity = do
  a <- manyTill anyChar commentOrNewLine
  skipComments
  return $ strip $ pack a
  where
    commentOrNewLine = (try $ lookAhead $ string "--") <|>
                       (try $ newline >> return "") <|>
                       (eof >> return "")

-- | Parses activity record, skipping comments before and afterwards.
parseActivityRecord :: Parser ActivityRecord
parseActivityRecord = do
  skipComments
  a <- liftA3 ActivityRecord parseTime parseActivity lookAheadEndTime
  skipComments
  return a

-- | Parses one day of the activity log.
parseDayLog :: Parser (DT.Day, [ActivityRecord])
parseDayLog =
  skipEmpty >> skipComments >> skipEmpty >>
  char '#' >> skipMany space >>
  liftA2 (,) parseDay (many parseActivityRecord)

-- | Parses an activity log.
parseLog :: Parser Log
parseLog = (Log . DM.fromList) <$> many parseDayLog

main = do
  let pt = parseByteString parseTime mempty
  print $ pt "12:00"
  print $ pt "ab"
  print $ pt "33:22"
  print $ pt "01:11"
  print $ pt "00:00"
  print $ pt "12:77"
  print $ pt "12:59"
  ---
  let pr = parseByteString (many parseActivityRecord) mempty
  print $ pr "08:00 Breakfast -- should I try skippin bfast?\n09:33 Activity"
  print $ pr "09:33 Activity\nAnd something else"
  print $ pr "99:99 Something"
  print $ pr "Other 99:99"
  ---
  let pd = parseByteString parseDayLog mempty
  print $ pd "--some comments\n# 2027-09-19\n08:00 Breakfast --comments\n09:00 Sanitizing moisture collector"
  ---
  let pl = parseByteString parseLog mempty
  print $ pl "--some comments\n# 2027-09-19 -- dates not nececessarily sequential\n08:00 Breakfast --comments\n09:00 Sanitizing moisture collector\n\n# 2027-09-20\n08:00 Breakfast --comments\n09:00 Sanitizing moisture collector"
  print $ pl theLogExample
