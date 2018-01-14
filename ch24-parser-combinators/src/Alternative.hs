module Alternative where

import           Control.Applicative
import qualified QuasiQuotes as Q
import           Text.Trifecta

-- > parseString (some letter) mempty "bb"
-- Success "bb"

-- > parseString integer mempty "123"
-- Success 123

type NumberOrString =
  Either Integer String

a = "bb"
b = "123"
c = "123bb123"

parseNos :: Parser NumberOrString
parseNos =
  (Left <$> integer) <|> (Right <$> some letter)

-- > :t some letter
-- some letter: CharParsing f => f [Char]
--
-- > data MyName = MyName String deriving Show
-- > :t MyName <$> (some letter)
-- MyName <$> (some letter) :: Parser MyName
--
-- > parseString (some letter) mempty "Cc"
-- Success "Cc"
--
-- > parseString (MyName <$> (some letter)) mempty "Cc"
-- Success (MyName "Cc")

parseNos' :: Parser NumberOrString
parseNos' =
  skipMany (oneOf "\n") >>
  (Left <$> integer) <|> (Right <$> some letter)

parseNos'' :: Parser NumberOrString
parseNos'' = do
  skipMany (oneOf "\n")
  v <- (Left <$> integer) <|> (Right <$> some letter)
  skipMany (oneOf "\n")
  return v

main = do
  let p f i =
        parseString f mempty i
  print $ p (some letter) a
  print $ p integer b
  print $ p parseNos a
  print $ p parseNos b
  print $ p (many parseNos) c  -- 0+
  print $ p (some parseNos) c  -- 1+

  print $ p parseNos Q.eitherOr
  print $ p parseNos' Q.eitherOr
  print $ parseString (some parseNos') mempty Q.eitherOr
  print $ parseString (some parseNos'') mempty Q.eitherOr
  print $ parseString (some (token parseNos')) mempty Q.eitherOr


-- > parseString (some integer) memtpy "123"
-- Success [123]

-- > parseString (many integer) memtpy "123"
-- Success [123]

-- > parseString (many integer) mempty ""
-- Success []

-- > parseString (some integer) mempty ""
-- Failure


{-

class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  some v = some_v
    where
      many_v = some_v <|> pure []
      some_v = (fmap (:) v) <*> many_v

  many :: f a -> f [a]
  many v = many_v
    where
      many_v = some_v <|> pure []
      some_v = (fmap (:) v) <*> many_v

-}
