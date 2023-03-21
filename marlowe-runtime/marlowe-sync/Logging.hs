{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Sync.Database (DatabaseSelector(..), getDatabaseSelectorConfig)
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig, getDefaultConnectorLogConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Component
  ( ConfigWatcherSelector(ReloadConfig)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SelectorLogConfig
  , getDefaultLogConfig
  , prependKey
  , singletonFieldConfig
  )

data RootSelector f where
  MarloweSyncServer :: ConnectorSelector (Handshake MarloweSync) f -> RootSelector f
  MarloweHeaderSyncServer :: ConnectorSelector (Handshake MarloweHeaderSync) f -> RootSelector f
  MarloweQueryServer :: ConnectorSelector (Handshake MarloweQuery) f -> RootSelector f
  Database :: DatabaseSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  MarloweSyncServer sel -> prependKey "marlowe-sync-server" $ getConnectorSelectorConfig False False sel
  MarloweHeaderSyncServer sel -> prependKey "marlowe-header-sync-server" $ getConnectorSelectorConfig False False sel
  MarloweQueryServer sel -> prependKey "marlowe-query-server" $ getConnectorSelectorConfig False False sel
  Database sel -> prependKey "database" $ getDatabaseSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Map Text SelectorLogConfig
defaultRootSelectorLogConfig = fold
  [ getDefaultConnectorLogConfig getRootSelectorConfig MarloweSyncServer
  , getDefaultConnectorLogConfig getRootSelectorConfig MarloweHeaderSyncServer
  , getDefaultConnectorLogConfig getRootSelectorConfig MarloweQueryServer
  , getDefaultLogConfig getRootSelectorConfig $ Database GetTip
  , getDefaultLogConfig getRootSelectorConfig $ Database GetTipForContract
  , getDefaultLogConfig getRootSelectorConfig $ Database GetCreateStep
  , getDefaultLogConfig getRootSelectorConfig $ Database GetIntersectionForContract
  , getDefaultLogConfig getRootSelectorConfig $ Database GetIntersection
  , getDefaultLogConfig getRootSelectorConfig $ Database GetNextHeaders
  , getDefaultLogConfig getRootSelectorConfig $ Database $ GetNextSteps MarloweV1
  , getDefaultLogConfig getRootSelectorConfig $ Database GetHeaders
  , getDefaultLogConfig getRootSelectorConfig $ Database GetContractState
  , getDefaultLogConfig getRootSelectorConfig $ Database GetTransaction
  , getDefaultLogConfig getRootSelectorConfig $ Database GetTransactions
  , getDefaultLogConfig getRootSelectorConfig $ Database GetWithdrawal
  , getDefaultLogConfig getRootSelectorConfig $ Database GetWithdrawals
  , getDefaultLogConfig getRootSelectorConfig $ ConfigWatcher ReloadConfig
  ]
