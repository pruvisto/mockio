{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, NoImplicitPrelude, PackageImports #-}
module Control.Monad.Pause (PauseT, yield, stepPause, runPause) where

import "base" Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Trans

newtype PauseT m a = PauseT {stepPause :: m (Either (PauseT m a) a)}
type Pause = PauseT Identity

instance (Monad m) => Functor (PauseT m) where
  fmap = liftM

instance (Monad m) => Monad (PauseT m) where
  return x = lift (return x)
  (PauseT s) >>= f = PauseT $ s >>= either (return . Left . (>>= f)) (stepPause . f)

instance (Functor m, Monad m) => Applicative (PauseT m) where
  pure  = return
  (<*>) = ap

instance MonadTrans PauseT where
    lift k = PauseT (liftM Right k)

yield :: Monad m => PauseT m ()
yield = PauseT (return (Left (return ())))

runPause :: Monad m => PauseT m a -> m a
runPause m = stepPause m >>= either runPause return

{-
runPause :: Monad m => PauseT m a -> m a
runPause (PauseT m) = m >>= either runPause return
-}

instance MonadState s m => MonadState s (PauseT m) where
  get = lift get
  put = lift . put
  state = lift . state

