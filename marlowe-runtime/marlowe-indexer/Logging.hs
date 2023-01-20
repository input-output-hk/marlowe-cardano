{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerSelector, getMarloweIndexerSelectorConfig)
import Network.Protocol.Driver
  (ConnectSocketDriverSelector, SocketDriverConfigOptions(..), getConnectSocketDriverSelectorConfig)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekClient :: ConnectSocketDriverSelector RuntimeChainSeek f -> RootSelector f
  ChainQueryClient :: ConnectSocketDriverSelector (Query ChainSyncQuery) f -> RootSelector f
  App :: MarloweIndexerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-seek" $ getConnectSocketDriverSelectorConfig chainSeekConfig sel
  ChainQueryClient sel -> prependKey "chain-query" $ getConnectSocketDriverSelectorConfig chainQueryConfig sel
  App sel -> prependKey "marlowe-indexer" $ getMarloweIndexerSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

chainSeekConfig :: SocketDriverConfigOptions
chainSeekConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = False
  }

chainQueryConfig :: SocketDriverConfigOptions
chainQueryConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = False
  }
