module CharAndToken where

import Text.Trifecta

-- lexer :: Steram Char -> Stream Token
-- parser :: Stream Token -> AST

f0 = parseString digit mempty "123 456"
-- Success '1'

f1 = parseString (some digit) mempty "123 456"
-- Success "123"

f2 = parseString (some (some digit)) mempty "123 456"
-- Success ["123"]

f3 = parseString (some integer) mempty "123"
-- Success [123]

f4 = parseString (some integer) mempty "123456"
-- Success [123456]

f5 = parseString (some integer) mempty "123 456"
-- Success [123, 456]

f6 = parseString (some integer) mempty "123 \n \n 456"
-- Success [123, 456]

ts = "123 \n \n 456"

t0 = parseString (token (some digit)) mempty ts
-- Success "123"

t1 = parseString (some (token digit)) mempty ts
-- Success "123456"

t2 = parseString (token (some (token digit))) mempty ts
-- Success "123456"

t3' = parseString decimal mempty ts
-- Success 123

t3 = parseString (token (some decimal)) mempty ts
-- Succuss [123]

t4 = parseString (token (some (token decimal))) mempty ts
-- Success [123,456]


t5 = parseString (some integer) mempty "1\n2\n 3\n"
-- Success [1,2,3]
