module NewBegining where

import Control.Applicative

boop :: Num a => a -> a
boop = (*2)

doop :: Num a => a -> a
doop = (+10)


bip :: Num a => a -> a
bip = boop . doop -- \a -> boop (doop a)

bloop :: Num a => a -> a
bloop = fmap boop doop -- \a -> boop (doop a)

bbop :: Num a => a -> a
bbop = (+) <$> boop <*> doop
-- :t (<*>) :: (a -> a -> b) -> (a -> a) -> (a -> b)

duwop :: Num a => a -> a
duwop = liftA2 (+) boop doop
-- \a -> (boop a) + (doop a)

boopDoop :: Num a => a -> a
boopDoop = do
  a <- boop
  b <- doop
  return (a + b)
