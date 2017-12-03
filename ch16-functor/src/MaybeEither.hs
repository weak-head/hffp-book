module MaybeEither where


-- sad
incIfJust :: Num a => Maybe a -> Maybe a
incIfJust Nothing = Nothing
incIfJust (Just a) = Just (a + 1)

showIfJust :: Show a => Maybe a -> Maybe String
showIfJust Nothing = Nothing
showIfJust (Just a) = Just (show a)


-- using fmap
incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = fmap (+1) --eta-reduce: incMaybe m = fmap (+1) m

showMaybe :: Show a => Maybe a -> Maybe String
showMaybe = fmap show --eta-reduce: showMaybe m = fmap show m


-- Lifted
liftedInc :: (Num a, Functor f) => f a -> f a
liftedInc = fmap (+1)

liftedShow :: (Show a, Functor f) => f a -> f String
liftedShow = fmap show


data Po a =
    No
  | Ye a
  deriving (Show, Eq)

instance Functor Po where
  fmap _  No    = No
  fmap f (Ye a) = Ye (f a)


-- ===================================================================

-- another sad implementation
incIfRight :: Num a => Either e a -> Either e a
incIfRight (Left x)  = Left x
incIfRight (Right a) = Right (a + 1)

showIfRight :: Show a => Either e a -> Either e String
showIfRight (Left x) = Left x
showIfRight (Right a) = Right (show a)

-- fmap over Either
incEither :: Num a => Either e a -> Either e a
incEither = fmap (+1)

showEither :: Show a => Either e a -> Either e String
showEither = fmap show


data CoPro a b =
    Fs a
  | Sn b
  deriving (Show, Eq)

instance Functor (CoPro a) where
  fmap _ (Fs a) = Fs a
  fmap f (Sn b) = Sn (f b)
