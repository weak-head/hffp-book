module ChEx where

import Data.Functor.Identity
import Control.Monad
import Control.Monad.Trans.State

get' :: State s s
get' = state $ \s -> (s, s)
-- runState get' "abc"
-- > ("abc", "abc")

put' :: s -> State s ()
put' s = state $ const ((), s)
-- runState (put' "abc") "ooo"
-- > ((), "abc")

exec' :: State s a -> s -> s
exec' (StateT sa) = snd . runIdentity . sa
-- exec' (put' "abc") "ooo"
-- > "abc"
--
-- exec' get' "abc"
-- "abc"

eval' :: State s a -> s -> a
eval' (StateT sa) = fst . runIdentity . sa
-- eval' get' "abc"
-- > "abc"

modify' :: (s -> s) -> State s ()
modify' f = state $ \s -> ((), f s)
-- runState (modify' (+1)) 0
-- > ((), 1)
--
-- runState (modify' (+1) >> modify (+1)) 0
-- > ((), 2)
