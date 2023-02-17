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
import Network.Protocol.Driver
  ( ConnectSocketDriverSelector
  , ConnectionSourceSelector
  , SocketDriverConfigOptions(..)
  , getConnectSocketDriverSelectorConfig
  , getConnectionSourceSelectorConfig
  )
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  (ConfigWatcherSelector(ReloadConfig), GetSelectorConfig, SelectorConfig(..), prependKey, singletonFieldConfig)

data RootSelector f where
  ChainSeekClient :: ConnectSocketDriverSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainSyncJobClient :: ConnectSocketDriverSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ChainSyncQueryClient :: ConnectSocketDriverSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  HistoryClient :: ConnectSocketDriverSelector (Handshake MarloweSync) f -> RootSelector f
  Server :: ConnectionSourceSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectSocketDriverSelectorConfig chainSeekConfig sel
  ChainSyncJobClient sel -> prependKey "chain-sync-job" $ getConnectSocketDriverSelectorConfig chainSyncJobConfig sel
  ChainSyncQueryClient sel -> prependKey "chain-sync-query" $ getConnectSocketDriverSelectorConfig chainSyncQueryConfig sel
  HistoryClient sel -> prependKey "history" $ getConnectSocketDriverSelectorConfig historyClientConfig sel
  Server sel -> prependKey "server" $ getConnectionSourceSelectorConfig True True sel
  App sel -> getTransactionSererSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

chainSeekConfig :: SocketDriverConfigOptions
chainSeekConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }

chainSyncJobConfig :: SocketDriverConfigOptions
chainSyncJobConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }

chainSyncQueryConfig :: SocketDriverConfigOptions
chainSyncQueryConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }

historyClientConfig :: SocketDriverConfigOptions
historyClientConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = True
  }
