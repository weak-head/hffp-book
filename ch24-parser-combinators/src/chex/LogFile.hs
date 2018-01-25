{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module LogFile where

import qualified Data.Map as DM
import qualified Data.Time as DT
import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Text.RawString.QQ
import Text.Trifecta
import Text.Parser.LookAhead

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
--
-- > parseTimeM True defaultTimeLocale "%R" "12:00" :: Maybe TimeOfDay

type Time         = DT.TimeOfDay
type Activity     = String

newtype Log =
  Log { log :: DM.Map DT.Day DailyActivityLog }
  deriving (Eq, Show)

newtype DailyActivityLog =
  DailyActivityLog { dayLog :: DM.Map Time Activity }
  deriving (Eq, Show)

skipComments :: Parser ()
skipComments =
  skipMany (string "--" >>
            skipMany (noneOf "\n") >>
            skipMany (oneOf "\n"))

skipSpaces :: Parser ()
skipSpaces =
  skipMany (char ' ' <|> char '\n' <|> char '\t')

skipEol :: Parser ()
skipEol =
  skipMany (oneOf "\n")

parseTime :: Parser Time
parseTime = do
  hh <- fromIntegral <$> natural
  char ':'
  mm <- fromIntegral <$> natural
  case DT.makeTimeOfDayValid hh mm 0 of
     Nothing -> empty
     Just v  -> return v

parseActivity :: Parser Activity
parseActivity = do
  skipMany space
  a <- manyTill anyChar commentOrNewLine
  skipComments
  return a
  where
    commentOrNewLine = (try $ lookAhead $ string "--") <|>
                       (newline >> return "")
  -- TODO: look ahead for time

parseActivityRecord :: Parser (Time, Activity)
parseActivityRecord =
  liftA2 (,) parseTime parseActivity

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
  let pr = parseByteString parseActivityRecord mempty
  print $ pr "08:00 Breakfast -- should I try skippin bfast?"
  print $ pr "09:33 Activity\nAnd something else"
  print $ pr "99:99 Something"
  print $ pr "Other 99:99"
