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

-- | Parses IPv4 address.
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

-- | Parses full representation of IPv6 address.
-- Eg: FE80:0000:0000:0000:0202:B3FF:FE1E:8329
parseFullIPv6 :: Parser IPv6Address
parseFullIPv6 = liftA2 IPv6Address parseHexWord (char ':' >> parseHexWord)
  where
    parseHexWord = do
      h1 <- flip shiftL 48 <$> parseHex
      char ':'
      h2 <- flip shiftL 32 <$> parseHex
      char ':'
      h3 <- flip shiftL 16 <$> parseHex
      char ':'
      h4 <- parseHex
      return $ h1 .|. h2 .|. h3 .|. h4

-- | Parses short representation of IPv6 address.
-- Eg: FE80::0202:B3FF:FE1E:8329
parseShortIPv6 :: Parser IPv6Address
parseShortIPv6 = do
  h  <- some (try parseOptHex)
  string "::"
  h' <- (try $ many parseOptHex) <|> return []
  return $ wrap $ fill h h'
  where
    parseOptHex = optional (char ':') >> parseHex
    fill xs ys  =
      concat [ xs
             , replicate (8 - (length xs + length ys)) 0
             , ys
             ]
    wrap ipa = IPv6Address
               (shiftL (ipa!!0) 48 .|.
                shiftL (ipa!!1) 32 .|.
                shiftL (ipa!!2) 16 .|.
                shiftL (ipa!!3) 0)
               (shiftL (ipa!!4) 48 .|.
                shiftL (ipa!!5) 32 .|.
                shiftL (ipa!!6) 16 .|.
                shiftL (ipa!!7) 0)

-- | Parses IPv6 address.
parseIPv6 :: Parser IPv6Address
parseIPv6 = try parseFullIPv6 <|> parseShortIPv6

main = do
--  print $ parseString parseIPv4 mempty "172.16.254.1"
--  print $ parseString parseHex mempty "ffff"
  print $ parseString parseIPv6 mempty "FE80:0000:0000:0000:0202:B3FF:FE1E:8329"
  print $ parseString parseIPv6 mempty "FE80::0202:B3FF:FE1E:8329"
