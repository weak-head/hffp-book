module Laws where

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- return :: a -> m a


--- Identity ---------------------------------------------------------

rightIdentity :: (Monad m, Eq (m b)) => m b -> Bool
rightIdentity m =
  (m >>= return) == m

leftIdentity :: (Monad m, Eq (m b)) => (a -> m b) -> a -> Bool
leftIdentity f x =
  (return x >>= f) == f x

--- Associativity ----------------------------------------------------

associativity ::
  (Monad m, Eq (m b)) => m a1 -> (a1 -> m a) -> (a -> m b) -> Bool
associativity m f g =
  ((m >>= f) >>= g) == (m >>= (\x -> f x >>= g))
