{-#LANGUAGE OverloadedStrings #-}

module ParsersJazz where

import Text.Trifecta

-- > :t char
-- char :: CharParsing m => Char -> m Char

-- > :t parseString
-- parseString :: Parser a -> Text.Trifecta.Delta.Delta -> String -> Result a

gimmeA :: Parser Char
gimmeA = char 'a'

-- :t parseString gimmeA mempty
-- parseString gimmeA mempty :: String -> Result Char

-- > parseString gimmeA mempty "a"
-- Success 'a'

-- > parseString gimmeA mempty "b"
-- error

-- > parseString (char 'b') mempty "b"
-- Success 'b'

-- > parseString (char 'b' >> char 'c') mempty "b"
-- error

-- > parseString (char 'b' >> char 'c') mempty "bc"
-- Success 'c'

-- > parseString (char 'b' >> char 'c') mempty "abc"
-- error

-----

-- > parseString (string "abc") mempty "abc"
-- Success "abc"

-- > parseString (string "abc") mempty "bc"
-- error

-- > parseStrign (string "abc") mempty "ab"
-- error

-----

-- > parseString (char 'a') mempty "abcdef"
-- Success 'a'

stop :: Parser a
stop = unexpected "stop"

-- > parseString (char 'a' >> stop) mempty "abcdef"
-- error

-- > parseString (string "abc") mempty "abcdef"
-- Success "abc"

-- > parseString (string "abc" >> stop) mempty "abcdef"
-- error

----

-- > :t parseBytString
-- parseBytString
--   :: Parser a
--   -> Text.Trifecta.Delta.Delta
--   -> Data.ByteString.Internal.ByteString
--   -> Result a

-- > parseByteString (char 'a') mempty "a"
-- Success 'a'
