module Sand.Tree where

import Sand.ClassDef

data Tree a =
    Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

infixr 5 *+*

(*+*) :: Tree a -> Tree a -> Tree a
(*+*) = undefined

concat_ :: Tree (Tree a) -> Tree a
concat_ = foldr_ (*+*) Empty

instance Functor_ Tree where
  fmap_ _ Empty        = Empty
  fmap_ f (Leaf a)     = Leaf (f a)
  fmap_ f (Node l v r) = Node (fmap_ f l) (f v) (fmap_ f r)

instance Applicative_ Tree where
  pure_      = Leaf
  fs <*>! xs = concat_ $ fmap_ ($xs) (fmap_ fmap_ fs)

instance Monad_ Tree where
  return_  = pure_
  t >>=! f = concat_ $ fmap_ f t

instance Foldable_ Tree where
  foldMap_ _ Empty        = mempty_
  foldMap_ f (Leaf a)     = f a
  foldMap_ f (Node l v r) = foldMap_ f l `mappend_` f v `mappend_` foldMap_ f r

  foldr_ _ d Empty    = d
  foldr_ f d (Leaf v) = f v d
  foldr_ f d (Node l v r) =
    let d'  = foldr_ f d  l
        d'' = foldr_ f d' r
    in f v d''

instance Traversable_ Tree where
  traverse_ _ Empty    = pure_ Empty
  traverse_ f (Leaf a) = fmap_ Leaf (f a)
  traverse_ f (Node l v r)
    = fmap_ Node (traverse_ f l) <*>! (f v) <*>! (traverse_ f r)
