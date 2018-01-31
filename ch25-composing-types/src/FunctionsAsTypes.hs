module FunctionsAsTypes where

--
-- Identity :: * -> *
--
-- id :: a -> a
--
newtype Identity a =
  Identity { runIdentity :: a }

--
-- Compose :: (* -> *) -> (* -> *) -> * -> *
--
--     (.) :: (b -> c) -> (a -> b) -> a -> c
--
newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)
-- > Compose [Just 1, Nothing]
-- Compose {getCompose = [Just 1, Nothing]}
--
-- > let xs = [Just (1::Int), Nothing]
-- > :t Compose xs
-- Compose [Just (1::Int), Nothing] :: Compose [] Maybe Int
