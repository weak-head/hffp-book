module ParsingDecimalsOrFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

type DecimalOrFract =
  Either Integer Rational

testData =
  [ "1"
  , "1/2"
  , "12"
  , "12fs"
  , "1/2fs"
  , "3/47"
  , "fs"
  , "1.fs"
  , "f.1"
  , "1/f"
  , "1/0"
  , "0/2"
  ]

parseDec :: Parser Integer
parseDec = do
 i <- integer
 eof
 return i

parseRatio :: Parser Rational
parseRatio = do
  num <- decimal
  char '/'
  denom <- decimal
  eof
  case denom of
    0 -> fail "0 is not valid denomerator"
    _ -> return (num % denom)

parseDoF :: Parser DecimalOrFract
parseDoF =
   try (Left <$> parseDec) <|>
   try (Right <$> parseRatio)

main :: IO ()
main =
  mapM_ (print . parseString parseDoF mempty) testData
