
import Test.Hspec
import Test.QuickCheck

import qualified LibSpec as L

main :: IO ()
main = hspec $ do
  L.spec
