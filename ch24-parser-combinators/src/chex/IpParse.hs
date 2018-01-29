{-# LANGUAGE OverloadedStrings #-}

module IpParse where

import Data.Bits
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
  deriving (Eq, Ord)

-- | Renders IPv4 address as string.
instance Show IPv4Address where
  show (IPv4Address v) =
    concat [ show $ shiftR v 24 .&. 255
           , "."
           , show $ shiftR v 16 .&. 255
           , "."
           , show $ shiftR v 8  .&. 255
           , "."
           , show $ v .&. 255
           ]

-- | Parses a natural number from range 0-255.
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

-- | Parses IPv4 addresss
parseIPv4 :: Parser IPv4Address
parseIPv4 = do
  b1 <- flip shiftL 24 <$> parseByte
  char '.'
  b2 <- flip shiftL 16 <$> parseByte
  char '.'
  b3 <- flip shiftL 8 <$> parseByte
  char '.'
  b4 <- parseByte
  return $ IPv4Address $ b1 .|. b2 .|. b3 .|. b4

main = parseString parseIPv4 mempty "172.16.254.1"  
