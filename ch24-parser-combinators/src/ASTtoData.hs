{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ASTtoData where

-- Text -> Structure -> Meaning
--   parse -> unmarshal

-- Meaning -> Structure -> Text
--   marshal -> searialize

--import qualified Data.ByteString as BS
import           Control.Applicative
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.RawString.QQ
import           Data.Scientific (floatingOrInteger)

sectionJson :: ByteString
sectionJson = [r|
{ "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

-- > :t encode
-- encode :: ToJSON a => a -> LBS.ByteString
--
-- > :t decode
-- decode :: FromJSON a => LBS.ByteString -> Maybe a

-- > decode sectionJson :: Maybe Value
-- Just (Object (fromList [
-- ("whatisit",
--   Object (fromList [("red",
--                      String "intoothandclaw")])),
-- ("section",
--   Object (fromList [("host",
--                      String "wikipedia.org")]))]))

data TestData =
  TestData
  { section :: Host
  , what :: Color
  } deriving (Eq, Show)

newtype Host =
  Host String
  deriving (Eq, Show)

data Color =
    Red String
  | Blue String
  | Yellow String
  deriving (Eq, Show)


instance FromJSON TestData where
  parseJSON (Object v) = liftA2 TestData (v .: "section") (v .: "whatisit")
  parseJSON _          = fail "Expected an object for TestData"

instance FromJSON Host where
  parseJSON (Object v) = Host <$> v .: "host"
  parseJSON _          = fail "Expected an object for Host"

instance FromJSON Color where
  parseJSON (Object v) =
        (Red <$> v .: "red")
    <|> (Blue <$> v .: "blue")
    <|> (Yellow <$> v .: "yellow")
  parseJSON _ = fail "Expected an object for Color"

-- > :set -XOverloadedStrings
-- > decode "{\"blue\": \"123\"}" :: Maybe Color
-- Just (Blue "123")
--
-- > :set -XQuasiQuotes
-- > decode [r|{"red": "123"}|] :: Maybe Color
-- Just (Red "123")

-- --FromJSON
-- ByteString -> Value -> yourType
--  parse -> unmarshall
--
-- --ToJSON
-- yourtype -> Value -> ByteString
--   marshall -> serialize

data NumberOrString =
    Num Integer
  | Str Text
  deriving (Eq, Show)

instance FromJSON NumberOrString where
  parseJSON (Number i) = case floatingOrInteger i of
                           (Left _) -> fail "Must be integral number"
                           (Right integer) -> return $ Num integer
  parseJSON (String s) = return $ Str s
  parseJSON _ = fail "NumberOrString must be number or string"

decN :: ByteString -> Maybe NumberOrString
decN = decode

eitherDecN :: ByteString -> Either String NumberOrString
eitherDecN = eitherDecode

-- > :set -XOverloadedStrings
--
-- > decN "blah"
-- Nothing
--
-- > eitherDecN "blah"
-- Left "..not a json value"
--
-- > decN "\"blah\""
-- Just (Str "blah")
--
-- > decN "123"
-- Just (Num 123)

main = do
  let blah :: Maybe Value
      blah = decode sectionJson
  print blah
