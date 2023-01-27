{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Sync.Database (DatabaseSelector, getDatabaseSelectorConfig)
import Network.Protocol.Driver
  (AcceptSocketDriverSelector, SocketDriverConfigOptions(..), getAcceptSocketDriverSelectorConfig)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  MarloweSyncServer :: AcceptSocketDriverSelector MarloweSync f -> RootSelector f
  Database :: DatabaseSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  MarloweSyncServer sel -> prependKey "marlowe-sync-server" $ getAcceptSocketDriverSelectorConfig marloweSyncServerConfig sel
  Database sel -> prependKey "database" $ getDatabaseSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

marloweSyncServerConfig :: SocketDriverConfigOptions
marloweSyncServerConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }
