{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Contrib.Control.Monad.Trans.State.IO where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (StateT, evalStateT)
import Control.Monad.State.Class (MonadState (..))
import Control.Monad.Trans.Class (MonadTrans)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- `Control.Monad.State` is lazy so we are here as well.
newtype IOStateT s m a = IOStateT (StateT (IORef s) m a)
  deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)

instance (MonadIO m) => MonadState s (IOStateT s m) where
  get = IOStateT do
    ref <- get
    liftIO $ readIORef ref

  put s = IOStateT do
    ref <- get
    liftIO . writeIORef ref $ s

runIOStateT :: (MonadIO m) => s -> IOStateT s m a -> m (s, a)
runIOStateT s (IOStateT m) = do
  ref <- liftIO $ newIORef s
  a <- evalStateT m ref
  s' <- liftIO $ readIORef ref
  pure (s', a)

execIOStateT :: (MonadIO m) => s -> IOStateT s m a -> m a
execIOStateT s (IOStateT m) = do
  ref <- liftIO $ newIORef s
  evalStateT m ref

evalIOStateT :: (MonadIO m) => s -> IOStateT s m a -> m s
evalIOStateT s (IOStateT m) = do
  ref <- liftIO $ newIORef s
  void $ evalStateT m ref
  liftIO $ readIORef ref

unsafeExecIOStateT :: (MonadIO m) => IORef s -> IOStateT s m a -> m a
unsafeExecIOStateT ref (IOStateT m) = evalStateT m ref
