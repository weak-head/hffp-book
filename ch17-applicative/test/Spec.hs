import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

spec = parallel $ do
  describe "Applicative laws" $ do
    it "Identity" $ do
      2 `shouldBe` 2

-- applicative
--   :: ( Show a
--      , Show (m a)
--      , Show (m (a -> b))
--      , Show (m (b -> c))
--      , Applicative m
--      , CoArbitrary a
--      , EqProp (m a)
--      , EqProp (m b)
--      , EqProp (m c)
--      , Arbitrary a
--      , Arbitrary b
--      , Arbitrary (m a)
--      , Arbitrary (m (a -> b))
--      , Arbitrary (m (b -> c)))
--   => m (a, b, c) -> TestBatch


jqmain = quickBatch $ applicative xs
  where xs = [("b", "w", 1 :: Int)]


-- The value we are passing in, is used
-- just to know a type
type SSI = (String, String, Int)
momain = quickBatch $ applicative tr
  where
    tr :: [SSI]
    tr = undefined
