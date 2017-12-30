module Sequence where


sf :: (Monad m, Traversable t) => (a -> m b) -> t a -> m (t b)
sf = (sequence .) . fmap


{-

Lets follow the types:

 (.)  :: (b -> c) -> (a -> b) -> a -> c

 fmap :: (a -> b) -> f a -> f b

 sequence :: (Monad m, Traversable t)
          => t (m a) -> m (t a)

--

 (sequence .) :: (Monad m, Traversable t)
              => (a -> t (m b)) -> a -> m (t b)

 ((sequence .) .) :: (Monad m, Traversable t)
                  => (a -> b -> t (m c)) -> a -> b -> m (t c)



 >>

 (.)          :: (b -> c) -> (a -> b) -> a -> c

 (sequence .) :: (a -> t (m b)) -> (a -> m (t b))
                      <b>              <c>

 (fmap)       :: (a   ->   b)   -> (f a -> f b)
                      <a>              <b>

 >>


 ((sequence .) . fmap) :: (Monad m, Traversable t)
                       => (a -> m b) -> t a -> m (t b)

--

 so it is same as:
   (sequence .) . fmap =
      \f xs -> sequence (fmap f xs)


 in contrast:
   (sequence . fmap) =
      \f -> sequence (fmap f)


 similar example:
   (join .) . fmap

--

traverse ~ (sequence .) . fmap
(>>=)    ~ (join .) . fmap


-}
