module FunctorIO where

-- gitLine :: IO String
-- read :: Read a => String -> a

getInt :: IO Int
getInt = fmap read getLine

-- Control.Monad.void
constIO = fmap (const ()) getInt


fgetInt = fmap (+1) getInt

bumpInt = do
  int <- getInt
  return (int + 1)


fgetLine = fmap (++ " boofoo") getLine

boofoo = do
  input <- getLine
  return (input ++ " boofoo")
