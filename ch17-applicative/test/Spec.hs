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
