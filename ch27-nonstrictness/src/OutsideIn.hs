module OutsideIn where


-- This a problem for strict language.
outsideIn :: IO ()
outsideIn = do
  let x :: Int
      x = undefined
  s <- getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "Hello"


strictOutsideIn :: IO ()
strictOutsideIn = do
  let x :: Int
      x = undefined
  s <- getLine
  case x `seq` s of
    "hi" -> print x
    _    -> putStrLn "Hello"


strictOutsideIn' :: IO ()
strictOutsideIn' = do
  let x :: Int
      x = undefined
  s <- x `seq` getLine
  case s of
    "hi" -> print x
    _    -> putStrLn "Hello"

----------------------------------------------------------------------

-- const
wc x z =
  let y = undefined `seq` 'y'
  in x

r1 = foldr wc 'z' ['a'..'e']        -- 'a'
r2 = foldr (flip wc) 'z' ['a'..'e'] -- 'z'

---

bot = undefined

wc' x z =
  let y = bot `seq` 'y'
  in y `seq` x
-- undefined `seq` y `seq` x


r3 = foldr wc' 'z' ['a'..'e']        -- exception
r4 = foldr (flip wc') 'z' ['a'..'e'] -- exception

----------------------------------------------------------------------

adrift :: Int
adrift =
  let x = undefined
      y = 2
      z = (x `seq` y `seq` 10, 11)
  in snd z


----------------------------------------------------------------------

ex1 = const 1 undefined         -- 1

ex2 = const undefined 1         -- exception

ex3 = flip const undefined 1    -- 1

ex4 = flip const 1 undefined    -- exception

ex5 = const undefined undefined -- exception

ex6 = foldr const 'z' ['a'..'e'] -- 'a'

ex7 = foldr (flip const) 'z' ['a'..'e'] -- 'z'
