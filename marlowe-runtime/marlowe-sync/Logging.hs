{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Sync.Database (DatabaseSelector, getDatabaseSelectorConfig)
import Network.Protocol.Driver (ConnectionSourceSelector, getConnectionSourceSelectorConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  MarloweSyncServer :: ConnectionSourceSelector (Handshake MarloweSync) f -> RootSelector f
  MarloweHeaderSyncServer :: ConnectionSourceSelector (Handshake MarloweHeaderSync) f -> RootSelector f
  MarloweQueryServer :: ConnectionSourceSelector (Handshake MarloweQuery) f -> RootSelector f
  Database :: DatabaseSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  MarloweSyncServer sel -> prependKey "marlowe-sync-server" $ getConnectionSourceSelectorConfig True True sel
  MarloweHeaderSyncServer sel -> prependKey "marlowe-header-sync-server" $ getConnectionSourceSelectorConfig True True sel
  MarloweQueryServer sel -> prependKey "marlowe-query-server" $ getConnectionSourceSelectorConfig True True sel
  Database sel -> prependKey "database" $ getDatabaseSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True
