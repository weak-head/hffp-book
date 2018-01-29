{-# LANGUAGE OverloadedStrings #-}

module IpParse where

import Data.Foldable (foldl')
import Data.Char (digitToInt)
import Data.Bits
import Data.Word
import Text.Trifecta
import Control.Applicative

-- | Represents IPv4 address.
--
-- Typical examples:
--  * 172.16.254.1
--  * 204.120.0.15
--
newtype IPv4Address =
  IPv4Address Word32
  deriving (Eq, Ord)

-- | Represents IPv6 address.
--
-- Typical examples:
--  * 0:0:0:0:0:ffff:ac10:fe01
--  * FE80:0000:0000:0000:0202:B3FF:FE1E:8329
--  * 2001:DB8::8:800:200C:417A
--  * FE80::0202:B3FF:FE1E:8329
--
data IPv6Address =
  IPv6Address Word64 Word64
  deriving (Eq, Ord, Show)

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

-- | Parses a natural number (range 0-255).
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

-- | Parses a hexadecimal number (range 0-65535).
parseHex :: Parser Word64
parseHex = do
  hex <- number 16 hexDigit
  case validate hex of
    Nothing -> empty
    Just v  -> return v
  where
    validate n | n < 0     = Nothing
               | n > 65535 = Nothing
               | otherwise = Just $ fromInteger n
    number base baseDigit = do
      digits <- some baseDigit
      return $! foldl' (\x d -> base * x + toInteger (digitToInt d)) 0 digits

-- | Parses IPv4 addresss.
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

main = do
  print $ parseString parseIPv4 mempty "172.16.254.1"
  print $ parseString parseHex mempty "ff23"
