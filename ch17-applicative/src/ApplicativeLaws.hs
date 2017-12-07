{-# LANGUAGE TypeApplications #-}

module ApplicativeLaws where

-- Identity
-- > pure id <*> v = v

i1 = pure id <*> [1..5]
i2 = pure id <*> Just "Hello"
i3 = pure id <*> Nothing
i4 = pure id <*> Left "Error'ish"
i5 = pure id <*> Right 8001

i6 = pure id <*> (+1) $ 2

-- Composition
-- > pure (.) <*> u <*> v <*> w =
--     u <*> (v <*> w)

c1  = pure (.) <*> [(+1)] <*> [(*2)] <*> [1, 2, 3]
c1' = [(+1)] <*> ([(*2)] <*> [1, 2, 3])
c1e = c1 == c1'

c2  = pure (.) <*> Just (+1) <*> Just (*2) <*> Just 1
c2' = Just (+1) <*> (Just (*2) <*> Just 1)
c2e = c2 == c2'

-- Homomorphism
-- > pure f <*> pure x = pure (f x)

h1  = pure (+1) <*> pure @Maybe 1
h1' = pure @Maybe ((+1) 1)
h1e = h1 == h1'

h2  = pure (+1) <*> pure @[] 1
h2' = pure @[] ((+1) 1)
h2e = h2 == h2'

h3  = pure (+1) <*> pure @(Either String) 1
h3' = pure @(Either String) ((+1) 1)
h3e = h3 == h3'

-- Interchange
-- > u <*> pure y = pure ($ y) <*> u

in1  = Just (+2) <*> pure 2
    --      u    <*> pure y
    -- equals
    -- Just 4
in1' = pure ($2) <*> Just (+2)
--      ($ 2) :: Num a => (a -> b) -> b
-- Just (+ 2) :: Num a => Maybe (a -> a)

mPure :: a -> Maybe a
mPure = pure

embed :: Num a => Maybe ((a -> b) -> b)
embed = mPure ($2)

mApply :: Maybe ((a -> b) -> b)
       -> Maybe (a -> b)
       -> Maybe b
mApply = (<*>)
-- (<*>)  ::   f      (a     -> b) -> f        a     -> f     b
-- mApply :: Maybe ((a -> b) -> b) -> Maybe (a -> b) -> Maybe b

in1'' = pure ($2) `mApply` Just (+2)
in1e = (in1 == in1') && (in1' == in1'')


in2  = [(+1), (*2)] <*> pure 1
in2' = pure ($1) <*> [(+1), (*2)]
in2e = in2 == in2'

in3  = Just (+3) <*> pure 1
in3' = pure ($1) <*> Just (+3)
in3e = in3 == in3'
