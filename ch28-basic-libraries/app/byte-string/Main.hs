{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BL
import qualified Codec.Compression.GZip as GZip

input :: BL.ByteString
input = "123"

compressed :: BL.ByteString
compressed = GZip.compress input

main :: IO ()
main = do
  TIO.putStrLn $ TE.decodeUtf8 (s input)      -- 123
  TIO.putStrLn $ TE.decodeUtf8 (s compressed) -- exception
  where
    s = BL.toStrict
