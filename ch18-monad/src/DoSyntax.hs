module DoSyntax where

import Data.Maybe

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

-----

-- This won't work as expected:
-- > putStrLn <$> getLine
--
-- > :t putStrLn <$> getLine
-- putStrLn <$> getLine :: IO (IO ())
--
-- > :t getLine
-- getLine :: IO String
--
-- > :t putStrLn
-- putStrLn :: String -> IO ()
--
--
-- <$> :: Functor f => (a -> b) -> f a -> f b
--              ( a   -> b   )
-- putStrLn :: Strign -> IO ()
--

f :: Functor f => f String -> f (IO ())
f x = putStrLn <$> x

g :: (String -> b) -> IO b
g x = x <$> getLine

-- putStrLn <$> getLine :: IO (IO ())

--
fj = fromJust $ putStrLn <$> Just "abc"
fg = (++ "abc") <$> getLine


--  [1] [2] [3]
h :: IO (IO ())
h = putStrLn <$> getLine
-- [1] -> effect 'getLine' must perform to get a String
-- [2] -> effect that would be performed if 'putStrLn' was evaluated
-- [3] -> return of the 'putStrLn'
