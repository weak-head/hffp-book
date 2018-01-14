{-# LANGUAGE OverloadedStrings #-}

module ParsingFractions where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

badFraction    = "1/0"
alsoBad        = "10"
shouldWork     = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero" -- MonadFail
    _ -> return (numerator % denominator)

{---------

decimal :: Integral a => Parser a
char    :: Char -> Parser Char
(%)     :: Integral a => a -> a -> GHC.Real.Ratio a

parseSTring :: Parser a
            -> Text.Trifecta.Delta.Delta
            -> String
            -> Result a

-----------}


main :: IO ()
main = do
  let parseFraction' = parseString parseFraction mempty
  let parseInteger'  = parseString parseInteger mempty
  print $ parseFraction' badFraction
  print $ parseFraction' alsoBad
  print $ parseFraction' shouldWork
  print $ parseFraction' shouldAlsoWork

  print $ parseInteger' "123"
  print $ parseInteger' "123abc"

-- > parseString integer mempty "123abc"
-- Success 123

-- > parseString (integer >> eof) mempty "123abc"
-- error

-- > parseString (integer >> eof) mempty "123"
-- Success ()

parseInteger :: Parser Integer
parseInteger =
  integer >>=
  \i -> eof >> return i
