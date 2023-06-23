{-# LANGUAGE BlockArguments #-}

module Contrib.UnliftIO.Async.Pool where

import Control.Concurrent.Async.Pool (TaskGroup)
import Control.Concurrent.Async.Pool qualified as A
import Control.Exception (SomeException)
import UnliftIO (MonadUnliftIO (withRunInIO))

withTaskGroup :: (MonadUnliftIO m) => Int -> (TaskGroup -> m b) -> m b
withTaskGroup n f = withRunInIO \run -> A.withTaskGroup n (run . f)

mapTasks :: (Traversable t) => (MonadUnliftIO m) => TaskGroup -> t (m a) -> m (t a)
mapTasks p fs = withRunInIO \run -> A.mapTasks p (fmap run fs)

mapTasksE :: (Traversable t) => (MonadUnliftIO m) => TaskGroup -> t (m a) -> m (t (Either SomeException a))
mapTasksE p fs = withRunInIO \run -> A.mapTasksE p (fmap run fs)
