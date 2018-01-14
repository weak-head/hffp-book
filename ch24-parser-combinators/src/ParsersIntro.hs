module ParsersIntro where

import Text.Parser.Combinators
import Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one' :: Parser Char
one' = one >> stop

one :: Parser Char
one = char '1'

oneE :: Parser ()
oneE = char '1' >> eof

oneS :: Parser String
oneS = string "1"

twoS :: Parser String
twoS = string "2"

oneTwoS :: Parser String
oneTwoS = oneS >> twoS

{-

-- get :: Monad m => StateT s m s
-- put :: Monad m => s -> StateT s m ()
-- runStateT :: StateT s m a -> s -> m (a, s)

> runStateT (put 8) 7
((), 8)

> runStateT get 8
(8, 8)

> runStateT (put 1 >> get) 8
(1, 1)

> (runStateT $ put 1 >> get) 0
(1, 1)

> (runStateT $ put 2 >> get) 123
(2, 2)

> (runStateT $ put 2 >> return 9001) 0
(9001, 2)

-}

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = char '1' >>
          char '2' >>
          stop

oneTwoE :: Parser ()
oneTwoE = char '1' >> char '2' >> eof


stringPr :: String -> Parser String
stringPr = string


testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

testParseS :: (Show a) => String -> Parser a -> IO ()
testParseS s p =
  print $ parseString p mempty s


pNL s =
  putStrLn ('\n' : s)

main = do
  pNL "stop:"
  testParse stop

  pNL "one:"
  testParse one

  pNL "one':"
  testParse one'

  pNL "oneE 1:"
  testParseS "1" oneE

  pNL "oneE 12:"
  testParseS "12" oneE

  pNL "oneTwo:"
  testParse oneTwo

  pNL "oneTwo':"
  testParse oneTwo'

  pNL "oneTwoE:"
  testParseS "12" oneTwoE

  pNL "stringPr 1 :"
  testParseS "1" (stringPr "1")

  pNL "stringPr 12 :"
  testParseS "12" (stringPr "12")

  pNL "stringPr 123 :"
  testParseS "123" (stringPr "123")

  pNL "stringPr 123 :"
  testParseS "123" (stringPr "1")

  pNL "stringPr 123 :"
  testParseS "1" (stringPr "123")

  pNL "oneTwoS 12:"
  testParseS "12" oneTwoS

  pNL "oneTwoS 13:"
  testParseS "13" oneTwoS

  pNL "oneTwoS 123:"
  testParseS "123" oneTwoS
