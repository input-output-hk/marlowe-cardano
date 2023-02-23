{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Transaction (getTransactionSererSelectorConfig)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Server
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekClient :: ConnectorSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainSyncJobClient :: ConnectorSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ChainSyncQueryClient :: ConnectorSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  HistoryClient :: ConnectorSelector (Handshake MarloweSync) f -> RootSelector f
  Server :: ConnectorSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectorSelectorConfig True True sel
  ChainSyncJobClient sel -> prependKey "chain-sync-job" $ getConnectorSelectorConfig True True sel
  ChainSyncQueryClient sel -> prependKey "chain-sync-query" $ getConnectorSelectorConfig True True sel
  HistoryClient sel -> prependKey "history" $ getConnectorSelectorConfig True True sel
  Server sel -> prependKey "server" $ getConnectorSelectorConfig True True sel
  App sel -> getTransactionSererSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True
