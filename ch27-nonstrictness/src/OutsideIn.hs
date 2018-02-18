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
