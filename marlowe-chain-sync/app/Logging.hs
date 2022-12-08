{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  where

import Control.Concurrent.Component
import Control.Concurrent.STM
  ( STM
  , TQueue
  , TVar
  , atomically
  , flushTQueue
  , modifyTVar
  , newEmptyTMVarIO
  , newTQueue
  , newTQueueIO
  , newTVar
  , readTMVar
  , readTVar
  , tryPutTMVar
  , tryReadTMVar
  , writeTQueue
  )
import Control.Exception (Exception(displayException), SomeException)
import Control.Monad (guard, mfilter, unless)
import Control.Monad.Base (MonadBase(..))
import Data.Aeson (Key, Object, Value(..), toJSON)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable (asum, traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy.IO as LB
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeek)
import Network.Protocol.Driver (AcceptSocketDriverSelector)
import Observe.Event
import Observe.Event.Backend
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(..), RenderFieldJSON, RenderSelectorJSON)
import System.IO (stderr)

data RootSelector f where
  ChainSeekServer :: AcceptSocketDriverSelector RuntimeChainSeek f -> RootSelector f

instance DefaultRenderSelectorJSON RootSelector where
  defaultRenderSelectorJSON = \case
    ChainSeekServer sel -> first ("chain-seek." <>) $ defaultRenderSelectorJSON sel

logger :: Component IO () (EventBackend IO UUID RootSelector)
logger = component \_ -> do
  (pullEmitters, eventBackend) <- proxyEventBackend newOnceFlagMVar nextRandom
  let filteredEventBackend = eventBackend
  pure (runLogger defaultRenderSelectorJSON pullEmitters, filteredEventBackend)

data JSONEventState s = forall f. JSONEventState
  { emitter :: SelectorEmitter UUID s f
  , key :: Key
  , renderFieldJSON :: RenderFieldJSON f
  , fieldsVar :: TVar Object
  , parentsVar :: TVar (Set UUID)
  , causesVar :: TVar (Set UUID)
  }

runLogger :: forall s. RenderSelectorJSON s -> STM [SomeSelectorEmitter UUID s] -> IO ()
runLogger renderSelectorJSON pullEmitters = loop mempty
  where
    loop :: Map UUID (JSONEventState s) -> IO b
    loop state = do
      (nextAction, newState) <- atomically $ getNextAction state
      nextAction
      loop newState

    getNextAction :: Map UUID (JSONEventState s) -> STM (IO (), Map UUID (JSONEventState s))
    getNextAction state =
      asum $ ((mempty,) . Map.union state <$> pullEmittersAction) : (processEmitterAction state <$> Map.elems state)

    pullEmittersAction :: STM (Map UUID (JSONEventState s))
    pullEmittersAction = do
      newEmitters <- pullEmitters
      Map.fromList <$> traverse (\(SomeSelectorEmitter emitter) -> (ref emitter,) <$> emitterToJSONEventState (SomeSelectorEmitter emitter)) newEmitters

    processEmitterAction :: Map UUID (JSONEventState s) -> JSONEventState s -> STM (IO (), Map UUID (JSONEventState s))
    processEmitterAction state emitter = asum
      [ (mempty ,state) <$ addFieldAction emitter
      , (mempty ,state) <$ addParentAction emitter
      , (mempty ,state) <$ addProximateAction emitter
      , finishAction state emitter
      ]

    addFieldAction :: JSONEventState s -> STM ()
    addFieldAction JSONEventState{..} = fields emitter >>= traverse_ \field -> do
      let (fieldName, value) = renderFieldJSON field
      modifyTVar fieldsVar $ KeyMap.insert fieldName value

    addParentAction :: JSONEventState s -> STM ()
    addParentAction JSONEventState{..} = parents emitter >>= traverse_ (modifyTVar parentsVar . Set.insert)

    addProximateAction :: JSONEventState s -> STM ()
    addProximateAction JSONEventState{..} = causes emitter >>= traverse_ (modifyTVar causesVar . Set.insert)

    finishAction :: Map UUID (JSONEventState s) -> JSONEventState s -> STM (IO (), Map UUID (JSONEventState s))
    finishAction state JSONEventState{..} = do
      (endTime, mex) <- end emitter
      finalFields <- readTVar fieldsVar
      finalParents <- readTVar parentsVar
      finalCauses <- readTVar causesVar
      let
        metaEntries = KeyMap.fromList $ catMaybes
          [ Just ("_type", toJSON key)
          , Just ("_id", toJSON $ ref emitter)
          , Just ("start-time", toJSON $ start emitter)
          , Just ("end-time", toJSON endTime)
          , Just ("duration", toJSON $ diffUTCTime endTime $ start emitter)
          , ("parents",) . toJSON <$> (finalParents <$ guard (not $ Set.null finalParents))
          , ("proximate-causes",) . toJSON <$> (finalCauses <$ guard (not $ Set.null finalCauses))
          , ("exception",) . toJSON . displayException <$> mex
          ]
        msgJson = Object $ metaEntries <> finalFields
      pure (LB.hPutStrLn stderr $ encodeToLazyText msgJson, Map.delete (ref emitter) state)

    emitterToJSONEventState :: SomeSelectorEmitter UUID s -> STM (JSONEventState s)
    emitterToJSONEventState (SomeSelectorEmitter emitter) = do
      let (key, renderFieldJSON) = renderSelectorJSON $ selector emitter
      JSONEventState emitter key renderFieldJSON <$> newTVar mempty <*> newTVar mempty <*> newTVar mempty


-- TODO push below definitions upstream
filterEventBackend
  :: Monad m
  => (forall f. s f -> Maybe (f -> Bool))
  -> EventBackend m r s
  -> EventBackend m (Maybe r) s
filterEventBackend selectorPredicate =
  filterEventBackendM $ pure . (fmap . fmap) pure . selectorPredicate

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

data SomeSelectorEmitter r s = forall f. SomeSelectorEmitter (SelectorEmitter r s f)

data SelectorEmitter r s f = SelectorEmitter
  { ref :: r
  , start :: UTCTime
  , selector :: s f
  , fields :: STM [f]
  , parents :: STM [r]
  , causes :: STM [r]
  , end :: STM (UTCTime, Maybe SomeException)
  }

proxyEventBackend :: MonadBase IO m => m (OnceFlag m) -> m r -> STM (STM [SomeSelectorEmitter r s], EventBackend m r s)
proxyEventBackend newOnceFlag newReference = do
  queue <- newTQueue
  pure (flushTQueueNonEmpty queue, EventBackend
    { newEventImpl = \selector -> do
        ref <- newReference
        liftBase do
          start <- getCurrentTime
          fields <- newTQueueIO
          parents <- newTQueueIO
          causes <- newTQueueIO
          end <- newEmptyTMVarIO
          let
            unlessEnded m = do
              ended <- isJust <$> tryReadTMVar end
              unless ended m
            finish mex = do
              ended <- getCurrentTime
              void $ atomically $ tryPutTMVar end (ended, mex)
          atomically $ writeTQueue queue $ SomeSelectorEmitter SelectorEmitter
            { ref
            , start
            , selector
            , fields = flushTQueueNonEmpty fields
            , parents = flushTQueueNonEmpty parents
            , causes = flushTQueueNonEmpty causes
            , end = readTMVar end
            }
          pure EventImpl
            { referenceImpl = ref
            , addFieldImpl = liftBase . atomically . unlessEnded . writeTQueue fields
            , addParentImpl = liftBase . atomically . unlessEnded . writeTQueue parents
            , addProximateImpl = liftBase . atomically . unlessEnded . writeTQueue causes
            , finalizeImpl = liftBase $ finish Nothing
            , failImpl = liftBase . finish . Just
            }
    , newOnceFlag
    })

-- | Like flushTQueue but it retries if the result is empty
flushTQueueNonEmpty :: TQueue a -> STM [a]
flushTQueueNonEmpty = mfilter (not . null) . flushTQueue
