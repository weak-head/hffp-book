module EitherValidationSpec where

import qualified EitherValidation as E
import Control.Applicative
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


instance (Arbitrary a, Arbitrary b) => Arbitrary (E.Validation a b) where
  arbitrary =
--    oneof [liftA E.Success arbitrary, liftA E.Failure arbitrary]
    frequency [(1, E.Success <$> arbitrary)
              ,(1, E.Failure <$> arbitrary)]

instance (Eq a, Eq b) => EqProp (E.Validation a b) where
  (=-=) = eq

aplTst = quickBatch $ applicative a
  where
    a :: E.Validation String (Int, Int, Int)
    a = undefined
