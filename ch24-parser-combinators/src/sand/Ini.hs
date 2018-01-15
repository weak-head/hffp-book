{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Sand.Ini where

import Data.ByteString(ByteString)
import Control.Applicative ((<|>))
import Data.Map (Map, fromList)
import Text.RawString.QQ
import Text.Trifecta

type SectionHeader = String
type Key           = String
type Value         = String
type Assignments   = Map Key Value

newtype IniConfig =
  IniConfig (Map SectionHeader Assignments)
  deriving (Show, Eq)

skipSpaces :: Parser ()
skipSpaces =
  skipMany (char ' ' <|>
            char '\n' <|>
            char '\t')

skipComments :: Parser ()
skipComments =
  skipMany (do char ';' <|> char '#'
               skipMany (noneOf "\n")
               skipMany (oneOf "\n"))

skipEol :: Parser ()
skipEol = skipMany (oneOf "\n")

skipEmptyBlock :: Parser ()
skipEmptyBlock = skipSpaces >> skipComments >> skipEol

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

parseHeader :: Parser SectionHeader
parseHeader = parseBracketPair (some letter)

parseAssignment :: Parser (Key, Value)
parseAssignment = do
  skipEmptyBlock
  key <- some letter
  char '='
  value <- some (noneOf "\n;#")
  skipEmptyBlock
  return (key, value)

parseSection :: Parser (SectionHeader, Assignments)
parseSection = do
  skipEmptyBlock
  h <- parseHeader
  skipEol
  a <- some parseAssignment
  return (h, fromList a)

parseIni :: Parser IniConfig
parseIni = IniConfig . fromList <$> some parseSection

iniExample :: ByteString
iniExample = [r|
; ----------------------
; Auto-generated @ 10:34
; ----------------------

;; The set of implemented features
[features]

;; with the current state
discovery=ok
recovery=partial
backups=tbd

;; these features/protocols that
;; are not confirmed yet
xmmp=to be done
soap=under investigation

;; do not modify
[modules]
moda=on
modc=off

|]

main = parseByteString parseIni mempty iniExample
