module MaybeApplicative where

-- f ~ Maybe

type M = Maybe

-- (<*>) :: f (a -> b) -> f a -> f b
-- (<*>) :: M (a -> b) -> M a -> M b

-- pure :: a -> f a
-- pure :: a -> Maybe a


validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
  then Nothing
  else Just s


newtype Name =
  Name String deriving (Eq, Show)

newtype Address =
  Address String deriving (Eq, Show)


mkName :: String -> Maybe Name
mkName s =
  Name <$> validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a =
  Address <$> validateLength 100 a


data Person =
  Person Name Address
  deriving (Show, Eq)

mkPerson :: String -> String -> Maybe Person
mkPerson n a =
  case mkName n of
    Nothing -> Nothing
    Just n' ->
      case mkAddress a of
        Nothing -> Nothing
        Just a' ->
          Just $ Person n' a'

-- > :t fmap Person (mkName "Babe")
-- fmap Person (mkName "Babe") :: Maybe (Address -> Person)
--
-- > fmap (fmap Person (mkName "Babe"))
--        (mkAddress "old macdonalds's")
-- Couldn't match expected type 'Address -> b'
-- with actual type
--   'Maybe (Address -> Person)'
--
-- fmap  :: Functor f => (a -> b) -> f a -> f b
-- (<*>) :: Applicative f => f (a -> b) -> f a -> f b
--

-- > let s = "old macdonald's"
-- > let addy = mkAddress s
-- > let b = mkName "Babe"
-- > let person = fmap Person b
-- > person <*> addy
-- Just (Person (Name "Babe")
--              (Address "old macdonald's"))
--
-- > Person <$> mkName "Babe" <*> addy
-- Just (Person (Name "Babe")
--              (Address "old macdonald's"))

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a =
  Person <$> mkName n <*> mkAddress a



newtype Age =
  Age Int deriving (Eq, Show)

mkAge :: Int -> Maybe Age
mkAge a =
  if a < 0 || a > 120
  then Nothing
  else Just (Age a)

data PersonL =
  PersonL Name Address Age
  deriving (Show, Eq)

mkPersonL :: String
          -> String
          -> Int
          -> Maybe PersonL
mkPersonL n a g =
  PersonL <$> mkName n <*> mkAddress a <*> mkAge g


----------------------------------------

ex1 = const <$> Just "Hello" <*> pure "World"

ex2 = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> Just [1, 2, 3]
