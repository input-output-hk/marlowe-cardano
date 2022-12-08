{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Logging
  where

import Cardano.Api (ChainPoint(..), ChainTip(..))
import Control.Arrow (returnA)
import Control.Concurrent (threadDelay)
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
  , writeTVar
  )
import Control.Exception (Exception(displayException), SomeException, try)
import Control.Monad (forever, guard, join, mfilter, unless)
import Control.Monad.Base (MonadBase(..))
import Data.Aeson (FromJSON(..), Key, Object, ToJSON(..), Value(..), eitherDecodeFileStrict', object, withObject, (.=))
import Data.Aeson.Key (toText)
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Text (encodeToLazyText)
import Data.Bifunctor (Bifunctor(first))
import Data.Foldable (asum, traverse_)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as LB
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)
import Data.Void (absurd)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoBlockHeader, fromCardanoBlockHeaderHash, fromCardanoBlockNo, fromCardanoSlotNo)
import Language.Marlowe.Runtime.ChainSync (ChainSyncSelector(..))
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as NodeClient
import Network.Protocol.Driver (AcceptSocketDriverSelector(..), DriverSelector(..), RecvField(..), SendField(..))
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event
import Observe.Event.Backend
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(..), RenderFieldJSON, RenderSelectorJSON)
import System.Directory (canonicalizePath)
import System.FSNotify (Event(..), EventIsDirectory(..), watchDir, withManager)
import System.FilePath.Posix (dropFileName)
import System.IO (stderr)

data RootSelector f where
  ChainSeekServer :: AcceptSocketDriverSelector RuntimeChainSeek f -> RootSelector f
  QueryServer :: AcceptSocketDriverSelector (Query ChainSyncQuery) f -> RootSelector f
  JobServer :: AcceptSocketDriverSelector (Job ChainSyncCommand) f -> RootSelector f
  App :: ChainSyncSelector f -> RootSelector f
  ReloadConfig :: RootSelector (Either String LogConfig)

instance DefaultRenderSelectorJSON RootSelector where
  defaultRenderSelectorJSON = \case
    ChainSeekServer sel -> first ("chain-seek." <>) $ defaultRenderSelectorJSON sel
    QueryServer sel -> first ("query." <>) $ defaultRenderSelectorJSON sel
    JobServer sel -> first ("job." <>) $ defaultRenderSelectorJSON sel
    App sel -> renderChainSyncSelectorJSON sel
    ReloadConfig -> ("reload-log-config", ("config",) . toJSON)

renderChainSyncSelectorJSON :: RenderSelectorJSON ChainSyncSelector
renderChainSyncSelectorJSON = \case
  NodeClientEvent sel -> first ("node-client." <>) $ renderNodeClientSelectorJSON sel

renderNodeClientSelectorJSON :: RenderSelectorJSON NodeClient.NodeClientSelector
renderNodeClientSelectorJSON = \case
  NodeClient.Connect -> ("connect", absurd)
  NodeClient.Intersect -> ("intersect", \points -> ("points", toJSON $ pointToJSON <$> points))
  NodeClient.IntersectFound -> ("intersect-found", \point -> ("point", pointToJSON point))
  NodeClient.IntersectNotFound -> ("intersect-not-found", absurd)
  NodeClient.RollForward ->
    ( "roll-forward"
    , \case
        NodeClient.RollForwardBlock header -> ("block-header", toJSON $ fromCardanoBlockHeader header)
        NodeClient.RollForwardTip tip -> ("tip", tipToJSON tip)
        NodeClient.RollForwardNewCost cost -> ("new-cost", toJSON cost)
    )
  NodeClient.RollBackward ->
    ( "roll-backward"
    , \case
        NodeClient.RollBackwardPoint point -> ("point", pointToJSON point)
        NodeClient.RollBackwardTip tip -> ("tip", tipToJSON tip)
        NodeClient.RollBackwardNewCost cost -> ("new-cost", toJSON cost)
    )

pointToJSON :: ChainPoint -> Value
pointToJSON = \case
  ChainPointAtGenesis -> String "genesis"
  ChainPoint slotNo hash -> object
    [ "slotNo" .= fromCardanoSlotNo slotNo
    , "hash" .= fromCardanoBlockHeaderHash hash
    ]

tipToJSON :: ChainTip -> Value
tipToJSON = \case
  ChainTipAtGenesis -> String "genesis"
  ChainTip slotNo hash blockNo -> object
    [ "slotNo" .= fromCardanoSlotNo slotNo
    , "hash" .= fromCardanoBlockHeaderHash hash
    , "blockNo" .= fromCardanoBlockNo blockNo
    ]

logger :: Component IO FilePath (EventBackend IO (Maybe UUID) RootSelector)
logger = proc configFile -> do
  rec
    readConfig <- configWatcher -< (eventBackend, configFile)
    eventBackend <- logAppender -< readConfig
  returnA -< eventBackend

newtype LogConfig = LogConfig (Map Key (Maybe (Map Key Bool)))
  deriving newtype (Semigroup, Monoid)

instance FromJSON LogConfig where
  parseJSON = fmap LogConfig . withObject "LogConfig" (traverse parseSelectorConfig . KeyMap.toMap)
    where
      parseSelectorConfig = \case
        Bool False -> pure Nothing
        Bool True -> pure $ Just mempty
        val -> Just <$> parseJSON val

instance ToJSON LogConfig where
  toJSON (LogConfig config) = toJSON $ maybe (toJSON False) toJSON <$> config

configWatcher :: Component IO (EventBackend IO r RootSelector, FilePath) (STM LogConfig)
configWatcher = component \(eventBackend, configFile) -> do
  configVar <- newTVar defaultLogConfig
  let
    runConfigWatcher = do
      configFile' <- canonicalizePath configFile
      forever $ void $ try @SomeException $ withManager \manager -> do
        let dir = dropFileName configFile'
        let
          predicate = \case
            Added path _ IsFile -> path == configFile'
            Modified path _ IsFile -> path == configFile'
            Removed path _ IsFile -> path == configFile'
            _ -> False
          reloadConfig = void $ try @SomeException $ withEvent eventBackend ReloadConfig \ev -> do
            newConfig <- fmap (either (Left . displayException) id) . try @IOError $ eitherDecodeFileStrict' configFile
            addField ev newConfig
            traverse_ (atomically . writeTVar configVar . (<> defaultLogConfig)) newConfig
        reloadConfig
        _ <- watchDir manager dir predicate $ const reloadConfig
        forever $ threadDelay 1_000_000_000_000
  pure (runConfigWatcher, readTVar configVar)

defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig $ Just <$> Map.fromList
  [ ("chain-seek.connected", mempty)
  , ("chain-seek.disconnected", mempty)
  , ("query.connected", mempty)
  , ("query.disconnected", mempty)
  , ("query.send", Map.singleton "message" True)
  , ("query.recv", Map.singleton "message" True)
  , ("job.connected", mempty)
  , ("job.disconnected", mempty)
  , ("job.send", Map.singleton "message" True)
  , ("job.recv", Map.singleton "message" True)
  , ("reload-log-config", Map.singleton "config" True)
  , ("node-client.connect", mempty)
  , ("node-client.intersect", mempty)
  , ("node-client.intersect-found", Map.singleton "point" True)
  , ("node-client.intersect-not-found", mempty)
  , ("node-client.roll-backward", Map.fromList [("point", True), ("tip", True)])
  ]

logAppender :: Component IO (STM LogConfig) (EventBackend IO (Maybe UUID) RootSelector)
logAppender = component \config -> do
  (pullEmitters, eventBackend) <- proxyEventBackend newOnceFlagMVar nextRandom
  let filteredEventBackend = filterEventBackendM (flip filterRoot config) eventBackend
  pure (runLogger defaultRenderSelectorJSON pullEmitters, filteredEventBackend)

filterRoot :: RootSelector f -> STM LogConfig -> IO (Maybe (f -> IO Bool))
filterRoot = \case
  ChainSeekServer sel -> filterAcceptSocketDriver "chain-seek" sel
  QueryServer sel -> filterAcceptSocketDriver "query" sel
  JobServer sel -> filterAcceptSocketDriver "job" sel
  App sel -> filterChainSync sel
  ReloadConfig -> mkEventPredicate "" "reload-log-config" (const "config")

filterAcceptSocketDriver
  :: Key
  -> AcceptSocketDriverSelector ps f
  -> STM LogConfig
  -> IO (Maybe (f -> IO Bool))
filterAcceptSocketDriver prefix = \case
  Connected -> mkEventPredicate prefix "connected" absurd
  Disconnected -> mkEventPredicate prefix "disconnected" absurd
  ServerDriverEvent sel -> filterServerDriverEvent prefix sel

filterServerDriverEvent :: Key -> DriverSelector ps f -> STM LogConfig -> IO (Maybe (f -> IO Bool))
filterServerDriverEvent prefix = \case
  Send -> mkEventPredicate prefix "send" \case
    SendMessage _ _ -> "message"
  Recv -> mkEventPredicate prefix "recv" \case
    RecvMessage _ _ -> "message"
    StateBefore _ -> "stateBefore"
    StateAfter _ -> "stateAfter"

filterChainSync :: ChainSyncSelector f -> STM LogConfig -> IO (Maybe (f -> IO Bool))
filterChainSync = \case
  NodeClientEvent sel -> filterNodeClient "node-client" sel

filterNodeClient
  :: Key
  -> NodeClient.NodeClientSelector f
  -> STM LogConfig
  -> IO (Maybe (f -> IO Bool))
filterNodeClient prefix = \case
  NodeClient.Connect -> mkEventPredicate prefix "connect" absurd
  NodeClient.Intersect -> mkEventPredicate prefix "intersect" $ const "points"
  NodeClient.IntersectFound -> mkEventPredicate prefix "intersect-found" $ const "point"
  NodeClient.IntersectNotFound -> mkEventPredicate prefix "intersect-not-found" absurd
  NodeClient.RollForward -> mkEventPredicate prefix "roll-forward" \case
    NodeClient.RollForwardBlock _ -> "block-header"
    NodeClient.RollForwardTip _ -> "tip"
    NodeClient.RollForwardNewCost _ -> "new-cost"
  NodeClient.RollBackward -> mkEventPredicate prefix "roll-backward" \case
    NodeClient.RollBackwardPoint _ -> "point"
    NodeClient.RollBackwardTip _ -> "tip"
    NodeClient.RollBackwardNewCost _ -> "new-cost"

prepend :: Key -> Key -> Key
prepend prefix key = if T.null $ toText prefix then key else prefix <> "." <> key

mkEventPredicate :: Key -> Key -> (f -> Key) -> STM LogConfig -> IO (Maybe (f -> IO Bool))
mkEventPredicate prefix key fieldKey readConfig = do
  let key' = prepend prefix key
  LogConfig config <- atomically readConfig
  pure do
    _ <- join $ Map.lookup key' config
    pure \field -> atomically do
      -- read again, as it may have changed
      LogConfig config' <- readConfig
      pure case Map.lookup key' config' of
        Just (Just fieldConfig) -> fromMaybe False $ Map.lookup (fieldKey field) fieldConfig
        _ -> False

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
