module Common where

-- const :: a -> b -> a

-- :: b -> Char
replaceWithP = const 'p'


-- data Maybe a = Nothing | Just a

-- :: Maybe Char
replace1 = fmap replaceWithP (Just 10)

-- :: [Char]
replace2 = fmap replaceWithP [1..5]

-- :: [Char]
replace3  = fmap replaceWithP [] -- ""
-- :: [Int]
replace3' = fmap (+1) []  -- []

-- :: (Int, Char)
replace4 = fmap replaceWithP (7, 20)


--

fnegate = fmap (+1) negate -- (+1) . negate

--

stackingFunctors =
  let n = Nothing
      b = Just "boo"
      p = Just "foo"
      nbp = [n, b, p]
      r_nbp       = replaceWithP nbp      -- 'p'
      fmap_r_nbp  = fmap replaceWithP nbp -- "ppp"
      fmap2_r_nbp = (fmap . fmap) replaceWithP nbp -- [Nothing, Just 'p', Just 'p']
      fmap3_r_nbp = (fmap . fmap . fmap) replaceWithP nbp -- [Nothing, Just "ppp", Just "ppp"]
   in fmap3_r_nbp

----------------------------------------------------------------------
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- fmap :: (a -> b) -> (f a -> f b)
--
-- fmap :: Functor f => (a -> b) -> (f a -> f b)
-- fmap :: Functor g => (d -> e) -> (g d -> g e)
-- (fmap . fmap)
--
--   fmap . fmap
--
-- = ((a -> b) -> (f a -> f b)) . ((d -> e) -> (g d -> g e))
--       k            f k             s           f s ~ k
--       k    ->      m         .     s     ->    k
--
-- = ((g d -> g e) -> (f (g d) -> f (g e))) . ((d -> e) -> (g d -> g e))
--
-- = (d -> e) -> (f (g d) -> f (g e))
--
-- = (d -> e) -> f (g d) -> f (g e)
