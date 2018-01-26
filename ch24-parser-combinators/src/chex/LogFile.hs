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

type Time         = DT.TimeOfDay
type Activity     = Text

-- | The Activity Log.
newtype Log =
  Log { log :: DM.Map DT.Day [ActivityRecord] }
  deriving (Eq, Show)

-- | Represents a single Log record.
data ActivityRecord =
  ActivityRecord { startTime :: Time
                 , activity  :: Activity
                 , endTime   :: Time
                 } deriving (Eq, Show)

-- | Comment, till the end of line.
comment :: Parser String
comment =
  string "--" >>
  manyTill anyChar (void newline <|> eof)

-- | Space, tab or newline.
blank :: Parser Char
blank = space <|> newline <|> tab

-- | Skips a combination of comments and blank lines.
skipCommentsAndEmpty :: Parser ()
skipCommentsAndEmpty =
  skipMany (comment <|> (blank >> return ""))

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
  char '#'
  skipMany space
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
  skipCommentsAndEmpty
  return $ strip $ pack a
  where
    commentOrNewLine = (try $ lookAhead $ string "--") <|>
                       (try $ newline >> return "") <|>
                       (eof >> return "")

-- | Parses activity record, skipping comments before and afterwards.
parseActivityRecord :: Parser ActivityRecord
parseActivityRecord = do
  skipCommentsAndEmpty
  a <- liftA3 ActivityRecord parseTime parseActivity lookAheadEndTime
  skipCommentsAndEmpty
  return a

-- | Parses one day of the activity log.
parseDayLog :: Parser (DT.Day, [ActivityRecord])
parseDayLog = do
  skipCommentsAndEmpty
  a <- liftA2 (,) parseDay (many parseActivityRecord)
  skipCommentsAndEmpty
  return a

-- | Parses an activity log.
parseLog :: Parser Log
parseLog = (Log . DM.fromList) <$> many parseDayLog

main = print $ parseByteString parseLog mempty theLogExample

-----

theLogExample :: ByteString
theLogExample = [r|
-- a comment

-- another comment

-- and this is a comment as well

# 2025-02-05

-- Comments

-- More comments here

08:00 Breakfast
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
12:00 Lunch
-- The comment

-- And another one
13:00 Programming -- and here
17:00 Commuting home in rover
17:30 R&R


19:00 Dinner
21:00 Shower
21:15 Read
22:00 Sleep

-- Improve this

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

-- some comments here

-- and here

|]
