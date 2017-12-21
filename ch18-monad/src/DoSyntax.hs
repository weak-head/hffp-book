module DoSyntax where

-- (*>) :: Applicative f => f a -> f b -> f b
-- (>>) :: Monad m       => m a -> m b -> m b

f1  = putStrLn "H, " >> putStrLn "B"
f1' = putStrLn "H, " *> putStrLn "B"

sequencing' :: IO ()
sequencing' = do
  putStrLn "H, "
  putStrLn "B"

sequencing'' :: IO ()
sequencing'' =
  putStrLn "H, " >>
  putStrLn "B"

sequencing''' :: IO ()
sequencing''' =
  putStrLn "H, " *>
  putStrLn "B"

-----

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name

binding' :: IO ()
binding' =
  getLine >>= putStrLn
