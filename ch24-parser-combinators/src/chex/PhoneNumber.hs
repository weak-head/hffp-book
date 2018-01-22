module PhoneNumber where

import Control.Applicative
import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea Exchange LineNumber
  deriving (Eq, Show)

parseNumberingPlanArea :: Parser NumberingPlanArea
parseNumberingPlanArea =
  try (between (symbol "(") (symbol ")") pn) <|> pn
  where pn = read <$> count 3 digit

parseExchange :: Parser Exchange
parseExchange = do
  skipOptional (oneOf "- ")
  v <- read <$> count 3 digit
  skipOptional (oneOf "- ")
  return v

parseLineNumber :: Parser LineNumber
parseLineNumber = read <$> count 4 digit

parsePhone :: Parser PhoneNumber
parsePhone =
  liftA3 PhoneNumber parseNumberingPlanArea parseExchange parseLineNumber

main :: IO ()
main = do
  let p = parseString parsePhone mempty
  ---
  print $ p "123-456-7890"
  print $ p "1234567890"
  print $ p "(123) 456-7890"
  print $ p "1-123-456-7890"
    -- for all of them: PhoneNumber 123 456 7890
