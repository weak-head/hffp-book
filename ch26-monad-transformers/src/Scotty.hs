{-# LANGUAGE OverloadedStrings #-}

{-

newtype ScottyT e m a =
  ScottyT { runS :: State (ScottyState e m) a }
  deriving (Functor, Applicative, Monad)

newtype ActionT e m a =
  ActionT { runAM :: ExceptT (ActionError e) (ReaderT ActionEnv (StateT ScottyResponse m)) a }
  deriving ( Functor, Applicative )

type ScottyM = ScottyT Text IO
type ActionM = ActionT Text IO

-}

module Scotty where

import Control.Monad  (liftM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)
import Data.Monoid (mconcat)
import Data.Text.Internal.Lazy
import Web.Scotty
import Web.Scotty.Internal.Types -- (ActionT(..))

main = scotty 3000 $ do
  get "/:word" $ do
    beam <- param "word"
--
--    lift $ putStrLn "hello"
--
--    ActionT . lift . lift . lift $ putStrLn "hello"
--
--    (ActionT
--      . (ExceptT . liftM Right)
--      . lift
--      . lift) (putStrLn "hello")
--
--    (ActionT
--      . ExceptT . liftM Right
--      . ReaderT . const
--      . lift) (putStrLn "hello")
--
    (ActionT
      . ExceptT . liftM Right
      . ReaderT . const
      . \m -> StateT (\s -> do
                         a <- m
                         return (a, s))
      ) (putStrLn "hello")

    wrapFunc

    html $ mconcat [ "<h1>Scotty, "
                   , beam
                   , " me up!</h1>"]


{-
theFunc :: ActionEnv
        -> ScottyResponse
        -> IO (Either (ActionError String) Int, ScottyResponse)
theFunc = undefined
-}
wrapFunc :: ActionT Text IO ()
wrapFunc = actionWrap
  where

    actionWrap = ActionT exceptWrap

    ------

    stateWrap :: StateT ScottyResponse IO ()
    stateWrap = StateT $ \s -> putStrLn "hello" >> return ((), s)
--  stateWrap = StateT stateF


    readerWrap :: ReaderT ActionEnv (StateT ScottyResponse IO) ()
    readerWrap = ReaderT $ \a -> StateT $ \s -> putStrLn "hello" >> return ((), s)
--  readerWrap = ReaderT $ \a -> StateT stateF
--  readerWrap = ReaderT readerF

    exceptWrap :: ExceptT
                    (ActionError Text)
                    (ReaderT ActionEnv (StateT ScottyResponse IO))
                    ()
    exceptWrap = ExceptT $ liftM Right $ ReaderT $ \a -> StateT $ \s -> putStrLn "hello" >> return ((), s)
--  exceptWrap = ExceptT $ ReaderT $ \a -> StateT $ \s -> theActualFunc a s
--  exceptWrap = ExceptT $ liftM Right $ ReaderT $ \a -> StateT stateF
--  exceptWrap = ExceptT exceptF

    ------

    theActualFunc :: ActionEnv -> ScottyResponse -> IO (Either (ActionError Text) (), ScottyResponse)
    theActualFunc a s = putStrLn "hello" >> return (Right (), s)

    ------
    stateF :: (Monad m) => ScottyResponse -> m (a, ScottyResponse)
    stateF = undefined

    readerF :: (Monad m) => ActionEnv -> m a
    readerF = undefined

    exceptF :: (Monad m) => m (Either (ActionError String) a)
    exceptF = undefined

{-
lift :: (Monad m, MonadTrans t) => m a -> t m a
lift :: (MonadTrans t) => IO a -> t IO a
lift :: IO a -> ActionM a
lift :: IO () -> ActionM ()

-- three lifts, one each for ExceptT, ReaderT and StateT.
instance MonadTrans (ActionT e) where
  lift = ActionT . lift . lift . lift

instance MonadTrans (ExceptT e) where
  lift = ExceptT . liftM Right

instance MonadTrans (ReaderT r) where
  lift = liftReaderT

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> do
      a <- m
      return (a, s)

-}
