{-# LANGUAGE Strict #-}
-- {-# LANGUAGE StrictData #-}

module StrictAndStrictData where

blah x = 1
-- same as:
--   blah !x = 1
-- or
--   blah x = x `seq` 1


willForce x     = 1

willNotForce ~x = 1

f1 = willForce undefined
-- exception

f2 = willNotForce undefined
-- 1
