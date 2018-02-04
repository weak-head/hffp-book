module EitherT where

-- either
import Data.Either.Combinators (swapEither)

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT m) = EitherT $ (fmap . fmap) f m

instance Applicative m => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT f) <*> (EitherT v) = EitherT $ (<*>) <$> f <*> v

instance Monad m => Monad (EitherT e m) where
  return = pure
  (EitherT m) >>= f =
    EitherT $ do
      v <- m
      case v of
        Left  e -> return $ Left e
        Right a -> runEitherT $ f a

swapEitherT :: Functor m => EitherT e m a -> EitherT a m e
swapEitherT (EitherT m) = EitherT $ fmap swapEither m
