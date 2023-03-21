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
import Control.Monad (filterM, join, mfilter, unless)
import Control.Monad.Base (MonadBase, liftBase)
import Data.Maybe (catMaybes, isJust, maybeToList)
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
filterEventBackendM selectorPredicate EventBackend{..} = EventBackend
  { newEvent = \args@NewEventArgs{..} -> selectorPredicate newEventSelector >>= \case
      Nothing -> pure Event
        { reference = Nothing
        , addField = const $ pure ()
        , finalize = \case
            Nothing -> pure ()
            Just ex -> do
              Event{..} <- newEvent args
                { newEventInitialFields = mempty
                , newEventParent = join newEventParent
                , newEventCauses = catMaybes newEventCauses
                }
              finalize $ Just ex
        }
      Just fieldPredicate -> do
        filteredInitFields <- filterM fieldPredicate newEventInitialFields
        Event{..} <- newEvent $ args
          { newEventInitialFields = filteredInitFields
          , newEventParent = join newEventParent
          , newEventCauses = catMaybes newEventCauses
          }
        pure Event
          { reference = Just reference
          , addField = \field -> fieldPredicate field >>= \case
              False -> pure ()
              True -> addField field
          , finalize
          }
  , emitImmediateEvent = \args@NewEventArgs{..} -> selectorPredicate newEventSelector >>= \case
      Nothing -> pure Nothing
      Just fieldPredicate -> do
        filteredInitFields <- filterM fieldPredicate newEventInitialFields
        fmap Just . emitImmediateEvent $ args
          { newEventInitialFields = filteredInitFields
          , newEventParent = join newEventParent
          , newEventCauses = catMaybes newEventCauses
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
  => m r
  -> STM (STM [Some (EventRecord r s)], EventBackend m r s)
proxyEventBackend newReference = do
  queue <- newTQueue
  pure (flushTQueueNonEmpty queue, EventBackend
    { newEvent = \NewEventArgs{..} -> do
        ref <- newReference
        liftBase do
          start <- getCurrentTime
          fieldsVar <- newTVarIO newEventInitialFields
          endedVar <- newEmptyTMVarIO
          let
            unlessEnded m = do
              ended <- isJust <$> tryReadTMVar endedVar
              unless ended m
            parents = maybeToList newEventParent
            causes = newEventCauses
            selector = newEventSelector
          pure Event
            { reference = ref
            , addField = liftBase . atomically . unlessEnded . modifyTVar fieldsVar . (:)
            , finalize = \exception -> liftBase do
                end <- getCurrentTime
                atomically $ unlessEnded do
                  fields <- readTVar fieldsVar
                  writeTQueue queue $ Some EventRecord{..}
                  putTMVar endedVar ()
            }
    , emitImmediateEvent = \_ -> newReference
    })

-- | Like flushTQueue but it retries if the result is empty
flushTQueueNonEmpty :: TQueue a -> STM [a]
flushTQueueNonEmpty = mfilter (not . null) . flushTQueue
