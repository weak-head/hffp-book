module IntParser where

import Control.Applicative
import Data.Foldable (foldl')
import Data.Char (digitToInt)
import Text.Trifecta

strToInt :: String -> Int
strToInt = foldl' inc 0
  where inc a v = a * 10 + digitToInt v

parseDigit :: Parser Char
parseDigit = oneOf "0123456789"

base10Int :: Parser Int
base10Int = do
  v <- try (const (-1) <$> char '-') <|> pure 1
  (* v) . read <$> some parseDigit
-- to follow the exercise requirements, we need to use
-- strToInt instead of 'read' here

main :: IO ()
main = do
  let pd = parseString parseDigit mempty
  let pi = parseString base10Int mempty
  ---
  print $ pd "123"
  print $ pd "abc"
  ---
  print $ pi "123"
  print $ pi "123abc"
  print $ pi "abc"
  ---
  print $ pi "-123"
  print $ pi "-123abc"
  print $ pi "-abc"
