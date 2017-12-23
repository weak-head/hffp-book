module SimpleMonadExamples where

import Data.Bool

-------- List --------------------------------------------------------
-- (>>=) :: Monad m
--        => m  a -> (a ->  m  b) ->  m  b
-- (>>=) :: [ ] a -> (a -> [ ] b) -> [ ] b
-- (>>=) :: [a] -> (a -> [b]) -> [b]

-- return :: Monad m => a ->  m  a
-- return ::            a -> [ ] a
-- return ::            a -> [a]

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else [x*x]

twiceWhenEvenP :: [Integer] -> [Integer]
twiceWhenEvenP = (=<<) $ \x -> bool [x*x] [x*x, x*x] (even x)

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []

----------- Maybe ----------------------------------------------------

-- (>>=) :: Monad m
--       => m     a -> (a -> m     b) -> m     b
-- (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b

-- return :: Monad m => a -> m a
-- return :: a -> Maybe a

data Cow =
  Cow
  { name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty ""  = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n > 0 = Just n
             | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
  let w = weight c
      n = name c
  in if n == "Bess" && w > 499
     then Nothing
     else Just c

mkSphericalCow :: String
               -> Int
               -> Int
               -> Maybe Cow
mkSphericalCow name' age' weight' =
  case noEmpty name' of
    Nothing -> Nothing
    Just nammy ->
      case noNegative age' of
        Nothing -> Nothing
        Just agey ->
          case noNegative weight' of
            Nothing -> Nothing
            Just weighty ->
              weightCheck (Cow nammy agey weighty)

-- We cannot do this with 'Applicative', because
-- 'weightCheck' depends on the prior existence of a
-- 'Cow' value and return more manadic structure.
mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
  nammy <- noEmpty name'
  agey <- noNegative age'
  weighty <- noNegative weight'
  weightCheck (Cow nammy agey weighty)

-- > mkSphericalCow' "Bess" 5 500
-- Nothing
--
-- > mkSphericalCow' "Bess" 5 499
-- Just (Cow {name = "Bess", age = 5, weight = 499})

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
  noEmpty name' >>=
  \nammy ->
    noNegative age' >>=
    \agey ->
      noNegative weight' >>=
      \weighty ->
        weightCheck (Cow nammy agey weighty)
