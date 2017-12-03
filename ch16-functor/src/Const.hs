module Const where

-- const :: a -> b -> a

-- phantom type
-- Constant :: * -> * -> *
newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Show, Eq)

-- lawfull const functor instance
instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a


-- 2
f1 = const 2 (getConstant (Constant 3))

-- Constant { getConstant = 3 }
f2 = fmap (const 2) (Constant 3)

-- 3
f3 = getConstant $ fmap (const 2) (Constant 3)

-- 3
f4 = getConstant $ fmap (const "blah") (Constant 3)

----------------------------------------------------------------------

----------
-- Laws:
----------

-- Identity:
constIdentity =
  getConstant (id (Constant 3)) == getConstant (fmap id (Constant 3))

-- Composition of the const function:
constFuncCompose = let c1 = ((const 3) . (const 5)) 10 -- 3
                       c2 = ((const 5) . (const 3)) 10 -- 5
                   in c1 == c2 -- false

-- Composition:
constCompose = let c   = const 3
                   c'  = const 5
                   fc  = fmap c
                   fc' = fmap c'
                   separate = fc . fc'
                   fused    = fmap (c . c')
                   cw = Constant "foo"
                   dw = Constant "boo"
                   v1 = getConstant $ separate $ cw -- foo
                   v2 = getConstant $ fused $ dw -- boo
               in v1 == v2 -- false
