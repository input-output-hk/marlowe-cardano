{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Observe.Event.Component
  ( ConfigWatcherSelector(..)
  , FieldConfig(..)
  , GetSelectorConfig
  , LoggerDependencies(..)
  , SelectorConfig(..)
  , SelectorLogConfig(..)
  , SomeJSON(..)
  , absurdFieldConfig
  , getDefaultLogConfig
  , logger
  , prependKey
  , singletonFieldConfig
  , singletonFieldConfigWith
  ) where

import Control.Arrow (returnA, (&&&))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException, displayException)
import Control.Exception.Lifted (try)
import Control.Monad (forever, mfilter, void)
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Control.Monad.With (MonadWithExceptable)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), eitherDecodeFileStrict')
import Data.Aeson.Text (encodeToLazyText)
import Data.Foldable (traverse_)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Some (Some, withSome)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import Data.Time (diffUTCTime)
import Data.Void (Void, absurd)
import Observe.Event.Backend (InjectSelector, narrowEventBackend)
import Observe.Event.Backend.Extra
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import System.Directory (canonicalizePath)
import System.FSNotify (Event(..), EventIsDirectory(..), watchDir, withManager)
import System.FilePath (dropFileName)

type GetSelectorConfig s = forall f. s f -> SelectorConfig f

prependKey :: Text -> SelectorConfig f -> SelectorConfig f
prependKey prefix config@SelectorConfig{..} = config
  { key = prefix <> "." <> key
  }

data SelectorConfig f = SelectorConfig
  { key :: Text
  , defaultEnabled :: Bool
  , fieldConfig :: FieldConfig f
  }

absurdFieldConfig :: FieldConfig Void
absurdFieldConfig = FieldConfig absurd absurd absurd

data FieldConfig f = FieldConfig
  { fieldKey :: f -> Text
  , fieldDefaultEnabled :: f -> Bool
  , toSomeJSON :: f -> SomeJSON
  }

singletonFieldConfig :: ToJSON f => Text -> Bool -> FieldConfig f
singletonFieldConfig = singletonFieldConfigWith SomeJSON

singletonFieldConfigWith :: (f -> SomeJSON) -> Text -> Bool -> FieldConfig f
singletonFieldConfigWith toSomeJSON key enabled = FieldConfig
  { fieldKey = const key
  , fieldDefaultEnabled = const enabled
  , toSomeJSON = toSomeJSON
  }

type LogConfig = Map Text SelectorLogConfig

data SelectorLogConfig
  = SelectorDefault
  | SelectorDisabled
  | SelectorEnabled
  | SelectorEnabledWithFields (Map Text FieldLogConfig)

data FieldLogConfig
  = FieldDefault
  | FieldDisabled
  | FieldEnabled

instance FromJSON SelectorLogConfig where
  parseJSON = \case
    Bool True -> pure SelectorEnabled
    Bool False -> pure SelectorDisabled
    Null -> pure SelectorDefault
    v -> SelectorEnabledWithFields <$> parseJSON v

instance ToJSON SelectorLogConfig where
  toJSON = \case
    SelectorDefault -> Null
    SelectorDisabled -> Bool False
    SelectorEnabled -> Bool True
    SelectorEnabledWithFields fields -> toJSON fields

instance FromJSON FieldLogConfig where
  parseJSON = \case
    Bool True -> pure FieldEnabled
    Bool False -> pure FieldDisabled
    Null -> pure FieldDefault
    _ -> fail "Expected boolean or null"

instance ToJSON FieldLogConfig where
  toJSON = \case
    FieldDefault -> Null
    FieldDisabled -> Bool False
    FieldEnabled -> Bool True

getDefaultLogConfig
  :: GetSelectorConfig s
  -> s f
  -> Map Text SelectorLogConfig
getDefaultLogConfig getConfig sel = case getConfig sel of
  SelectorConfig key True _ -> Map.singleton key SelectorEnabled
  SelectorConfig key False _ -> Map.singleton key SelectorDisabled

data LoggerDependencies m r s = LoggerDependencies
  { configFilePath :: Maybe FilePath
  , getSelectorConfig :: GetSelectorConfig s
  , newRef :: m r
  , writeText :: TL.Text -> m ()
  , injectConfigWatcherSelector :: InjectSelector ConfigWatcherSelector s
  }

logger
  :: (MonadWithExceptable m, MonadBaseControl IO m, ToJSON r)
  => Component m (LoggerDependencies m r s) (EventBackend m (Maybe r) s)
logger = proc LoggerDependencies{..} -> case configFilePath of
  Nothing -> logAppender -< let getLogConfig = pure mempty in LogAppenderDependencies{..}
  Just configFile -> do
    rec
      getLogConfig <- configWatcher -< (narrowEventBackend injectConfigWatcherSelector eventBackend, configFile)
      eventBackend <- logAppender -< LogAppenderDependencies{..}
    returnA -< eventBackend

data ConfigWatcherSelector f where
  ReloadConfig :: ConfigWatcherSelector (Either String LogConfig)

configWatcher
  :: forall m r
   . (MonadWithExceptable m, MonadBaseControl IO m)
  => Component m (EventBackend m r ConfigWatcherSelector, FilePath) (STM LogConfig)
configWatcher = component \(eventBackend, configFile) -> do
  configVar <- newTVar mempty
  let
    runConfigWatcher :: m ()
    runConfigWatcher = do
      configFile' <- liftBase $ canonicalizePath configFile
      control \runInBase -> forever $ void $ try @_ @IOError $ withManager \manager -> do
        let dir = dropFileName configFile'
        let
          predicate = \case
            Added path _ IsFile -> path == configFile'
            Modified path _ IsFile -> path == configFile'
            Removed path _ IsFile -> path == configFile'
            _ -> False
          reloadConfig :: IO ()
          reloadConfig = void
            $ runInBase
            $ try @_ @SomeException
            $ withEvent eventBackend ReloadConfig \ev -> do
                newConfig <- liftBase
                  $ fmap (either (Left . displayException) id) . try @_ @IOError
                  $ eitherDecodeFileStrict' configFile
                addField ev newConfig
                liftBase $ traverse_ (atomically . writeTVar configVar) newConfig
        _ <- liftBase $ watchDir manager dir predicate $ const reloadConfig
        reloadConfig
        forever $ threadDelay 1_000_000_000_000
  pure (runConfigWatcher, readTVar configVar)

data LogAppenderDependencies m r s = LogAppenderDependencies
  { getLogConfig :: STM LogConfig
  , getSelectorConfig :: GetSelectorConfig s
  , newRef :: m r
  , writeText :: TL.Text -> m ()
  }

logAppender
  :: forall m r s
   . (MonadBase IO m, ToJSON r)
  => Component m (LogAppenderDependencies m r s) (EventBackend m (Maybe r) s)
logAppender = component \LogAppenderDependencies{..} -> do
  (pullEvents, eventBackend) <- proxyEventBackend newRef
  let
    eventFilter :: s f -> m (Maybe (f -> m Bool))
    eventFilter = configFilter getSelectorConfig $ liftBase $ atomically getLogConfig
  pure
    ( forever do
        events <- liftBase $ atomically pullEvents
        writeText $ TL.unlines $ encodeToLazyText . withSomeFlipped (renderEvent getSelectorConfig) <$> events
    , filterEventBackendM eventFilter eventBackend
    )

configFilter
  :: Monad m
  => GetSelectorConfig s
  -> m LogConfig
  -> (forall f. s f -> m (Maybe (f -> m Bool)))
configFilter getSelectorConfig getLogConfig selector = do
  let selectorConfig@SelectorConfig{..} = getSelectorConfig selector
  logConfigAtStart <- getLogConfig
  pure $ selectorLogConfigToFilter selectorConfig getLogConfig $ fromMaybe SelectorDefault $ Map.lookup key logConfigAtStart

selectorLogConfigToFilter
  :: Monad m
  => SelectorConfig f
  -> m LogConfig
  -> SelectorLogConfig
  -> Maybe (f -> m Bool)
selectorLogConfigToFilter config@SelectorConfig{..} getLogConfig = \case
  SelectorDefault
    | defaultEnabled -> selectorLogConfigToFilter config getLogConfig SelectorEnabled
    | otherwise -> selectorLogConfigToFilter config getLogConfig SelectorDisabled
  SelectorEnabled -> selectorLogConfigToFilter config getLogConfig $ SelectorEnabledWithFields mempty
  SelectorDisabled -> Nothing
  SelectorEnabledWithFields _ -> Just \field -> do
    logConfig <- getLogConfig
    pure $ selectorLogConfigToFieldFilter defaultEnabled fieldConfig field $ fromMaybe SelectorDefault $ Map.lookup key logConfig

selectorLogConfigToFieldFilter :: Bool -> FieldConfig f -> f -> SelectorLogConfig -> Bool
selectorLogConfigToFieldFilter defaultEnabled config@FieldConfig{..} field = \case
  SelectorDefault
    | defaultEnabled -> selectorLogConfigToFieldFilter defaultEnabled config field SelectorEnabled
    | otherwise -> selectorLogConfigToFieldFilter defaultEnabled config field SelectorDisabled
  SelectorEnabled -> selectorLogConfigToFieldFilter defaultEnabled config field $ SelectorEnabledWithFields mempty
  SelectorDisabled -> False
  SelectorEnabledWithFields fields -> case fromMaybe FieldDefault $ Map.lookup (fieldKey field) fields of
    FieldDefault -> fieldDefaultEnabled field
    FieldDisabled -> False
    FieldEnabled -> True

-- flip withSome doesn't work
withSomeFlipped :: (forall k. tag k -> a) -> Some tag -> a
withSomeFlipped f s = withSome s f

data SomeJSON = forall a. ToJSON a => SomeJSON a

instance ToJSON SomeJSON where
  toJSON (SomeJSON a) = toJSON a
  toEncoding (SomeJSON a) = toEncoding a

renderEvent :: ToJSON r => GetSelectorConfig s -> EventRecord r s f -> SomeJSON
renderEvent getSelectorConfig EventRecord{..} = SomeJSON $ Map.fromList $ meta <> fields'
  where
    SelectorConfig{..} = getSelectorConfig selector
    FieldConfig{..} = fieldConfig
    meta = catMaybes
      [ Just ("_type", SomeJSON key)
      , Just ("_id", SomeJSON ref)
      , Just ("start-time", SomeJSON start)
      , Just ("end-time", SomeJSON end)
      , Just ("duration", SomeJSON $ diffUTCTime end start)
      , ("parents",) . SomeJSON <$> mfilter (not . null) (pure parents)
      , ("proximate-causes",) . SomeJSON <$> mfilter (not . null) (pure causes)
      , ("exception",) . SomeJSON . displayException <$> exception
      ]
    fields' = (fieldKey &&& toSomeJSON) <$> fields
