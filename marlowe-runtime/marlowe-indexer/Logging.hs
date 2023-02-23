{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerSelector, getMarloweIndexerSelectorConfig)
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekClient :: ConnectorSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainQueryClient :: ConnectorSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  App :: MarloweIndexerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectorSelectorConfig False False sel
  ChainQueryClient sel -> prependKey "chain-query" $ getConnectorSelectorConfig True True sel
  App sel -> prependKey "marlowe-indexer" $ getMarloweIndexerSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True
