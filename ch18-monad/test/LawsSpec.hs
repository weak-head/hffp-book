module LawsSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = parallel $ do
  undefined

qmbatch = quickBatch (monad (undefined :: [(Int, Int, Int)]))
