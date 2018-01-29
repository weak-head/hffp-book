{-# LANGUAGE OverloadedStrings #-}

module IpParse where

import Data.Word
import Text.Trifecta
import Control.Applicative

-- | Represents the IPv4 address.
--
-- Typical examples:
--  * 172.16.254.1
--  * 204.120.0.15
--
newtype IPv4Address =
  IPv4Address Word32
  deriving (Eq, Ord, Show)

-- | Parses a number that is in range (0-255)
parseByte :: Parser Word32
parseByte = do
  n <- natural
  case validate n of
    Nothing -> empty
    Just v  -> return v
  where
    validate n | n < 0     = Nothing
               | n > 255   = Nothing
               | otherwise = Just $ fromIntegral n

main = parseString parseByte mempty "255"  
