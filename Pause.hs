{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, NoImplicitPrelude, PackageImports #-}
module Control.Monad.Pause (PauseT, pause, stepPause, runPauseT, runPause) where

import "base" Prelude
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer

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
  
instance (MonadError e m) => MonadError e (PauseT m) where
  throwError = lift . throwError
  catchError m f = PauseT $ catchError (stepPause m) (stepPause . f)

instance MonadState s m => MonadState s (PauseT m) where
  get = lift get
  put = lift . put
  state = lift . state

instance (MonadReader r m) => MonadReader r (PauseT m) where
  ask = lift ask
  local f m = PauseT $ local f $ stepPause m



runPauseT :: Monad m => PauseT m a -> m a
runPauseT m = stepPause m >>= either runPauseT return

runPause :: Pause a -> a
runPause = runIdentity . runPauseT

class Monad m => MonadPause m where
  pause :: m ()

instance Monad m => MonadPause (PauseT m) where
  pause = PauseT (return (Left (return ())))

instance (MonadPause m) => MonadPause (ReaderT s m) where
  pause = lift pause
 
instance (MonadPause m) => MonadPause (StateT s m) where
  pause = lift pause
 
instance (MonadPause m, Monoid w) => MonadPause (WriterT w m) where
  pause = lift pause
 
instance (MonadPause m) => MonadPause (ExceptT e m) where
  pause = lift pause

