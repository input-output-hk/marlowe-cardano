{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Observe.Event.Backend.Extra
  ( EventRecord(..)
  , filterEventBackend
  , filterEventBackendM
  , proxyEventBackend
  ) where

import Control.Concurrent.STM
  ( STM
  , TQueue
  , atomically
  , flushTQueue
  , modifyTVar
  , newEmptyTMVarIO
  , newTQueue
  , newTVarIO
  , putTMVar
  , readTVar
  , writeTQueue
  )
import Control.Concurrent.STM.TMVar (tryReadTMVar)
import Control.Exception (SomeException)
import Control.Monad (mfilter, unless)
import Control.Monad.Base (MonadBase, liftBase)
import Data.Maybe (isJust)
import Data.Some (Some(Some))
import Data.Time (UTCTime, getCurrentTime)
import Observe.Event.Backend

-- | Filter incoming events and fields.
filterEventBackend
  :: Monad m
  => (forall f. s f -> Maybe (f -> Bool))
  -> EventBackend m r s
  -> EventBackend m (Maybe r) s
filterEventBackend selectorPredicate =
  filterEventBackendM $ pure . (fmap . fmap) pure . selectorPredicate

-- | Filter incoming events and fields with a monadic action.
filterEventBackendM
  :: Monad m
  => (forall f. s f -> m (Maybe (f -> m Bool)))
  -> EventBackend m r s
  -> EventBackend m (Maybe r) s
filterEventBackendM selectorPredicate eb@EventBackend{..} = eb
  { newEventImpl = \selector -> selectorPredicate selector >>= \case
      Nothing -> pure EventImpl
        { referenceImpl = Nothing
        , addFieldImpl = const $ pure ()
        , addParentImpl = const $ pure ()
        , addProximateImpl = const $ pure ()
        , finalizeImpl = pure ()
        , failImpl = const $ pure ()
        }
      Just fieldPredicate -> do
        EventImpl{..} <- newEventImpl selector
        pure EventImpl
          { referenceImpl = Just referenceImpl
          , addFieldImpl = \field -> fieldPredicate field >>= \case
              False -> pure ()
              True -> addFieldImpl field
          , addParentImpl = maybe (pure ()) addParentImpl
          , addProximateImpl = maybe (pure ()) addProximateImpl
          , finalizeImpl
          , failImpl
          }
  }

-- | An aggregate summary of an event
data EventRecord r s f = EventRecord
  { selector :: s f
  , ref :: r
  , start :: UTCTime
  , fields :: [f]
  , parents :: [r]
  , causes :: [r]
  , end :: UTCTime
  , exception :: Maybe SomeException
  }

-- | Create an event backend that writes events to a queue for processing in a
-- separate thread.
proxyEventBackend
  :: MonadBase IO m
  => m (OnceFlag m)
  -> m r
  -> STM (STM [Some (EventRecord r s)], EventBackend m r s)
proxyEventBackend newOnceFlag newReference = do
  queue <- newTQueue
  pure (flushTQueueNonEmpty queue, EventBackend
    { newEventImpl = \selector -> do
        ref <- newReference
        liftBase do
          start <- getCurrentTime
          fieldsVar <- newTVarIO []
          parentsVar <- newTVarIO []
          causesVar <- newTVarIO []
          endedVar <- newEmptyTMVarIO
          let
            unlessEnded m = do
              ended <- isJust <$> tryReadTMVar endedVar
              unless ended m
            finish exception = do
              end <- getCurrentTime
              atomically $ unlessEnded do
                fields <- readTVar fieldsVar
                parents <- readTVar parentsVar
                causes <- readTVar causesVar
                writeTQueue queue $ Some EventRecord{..}
                putTMVar endedVar ()
          pure EventImpl
            { referenceImpl = ref
            , addFieldImpl = liftBase . atomically . unlessEnded . modifyTVar fieldsVar . (:)
            , addParentImpl = liftBase . atomically . unlessEnded . modifyTVar parentsVar . (:)
            , addProximateImpl = liftBase . atomically . unlessEnded . modifyTVar causesVar . (:)
            , finalizeImpl = liftBase $ finish Nothing
            , failImpl = liftBase . finish . Just
            }
    , newOnceFlag
    })

-- | Like flushTQueue but it retries if the result is empty
flushTQueueNonEmpty :: TQueue a -> STM [a]
flushTQueueNonEmpty = mfilter (not . null) . flushTQueue
