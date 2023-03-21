{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Transaction (getTransactionSererSelectorConfig)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import qualified Language.Marlowe.Runtime.Transaction.Query as Query
import Language.Marlowe.Runtime.Transaction.Server
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig, getDefaultConnectorLogConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  ( ConfigWatcherSelector(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SelectorLogConfig
  , getDefaultLogConfig
  , prependKey
  , singletonFieldConfig
  )

data RootSelector f where
  ChainSeekClient :: ConnectorSelector (Handshake RuntimeChainSeek) f -> RootSelector f
  ChainSyncJobClient :: ConnectorSelector (Handshake (Job ChainSyncCommand)) f -> RootSelector f
  ChainSyncQueryClient :: ConnectorSelector (Handshake (Query ChainSyncQuery)) f -> RootSelector f
  Server :: ConnectorSelector (Handshake (Job MarloweTxCommand)) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectorSelectorConfig False False sel
  ChainSyncJobClient sel -> prependKey "chain-sync-job" $ getConnectorSelectorConfig False False sel
  ChainSyncQueryClient sel -> prependKey "chain-sync-query" $ getConnectorSelectorConfig False False sel
  Server sel -> prependKey "server" $ getConnectorSelectorConfig False True sel
  App sel -> getTransactionSererSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

defaultRootSelectorLogConfig :: Map Text SelectorLogConfig
defaultRootSelectorLogConfig = fold
  [ getDefaultConnectorLogConfig getRootSelectorConfig ChainSeekClient
  , getDefaultConnectorLogConfig getRootSelectorConfig ChainSyncJobClient
  , getDefaultConnectorLogConfig getRootSelectorConfig ChainSyncQueryClient
  , getDefaultConnectorLogConfig getRootSelectorConfig Server
  , getDefaultLogConfig getRootSelectorConfig $ App Exec
  , getDefaultLogConfig getRootSelectorConfig $ App ExecCreate
  , getDefaultLogConfig getRootSelectorConfig $ App ExecApplyInputs
  , getDefaultLogConfig getRootSelectorConfig $ App ExecWithdraw
  , getDefaultLogConfig getRootSelectorConfig $ App $ LoadWalletContext Query.LoadWalletContext
  , getDefaultLogConfig getRootSelectorConfig $ App $ LoadMarloweContext Query.ContractNotFound
  , getDefaultLogConfig getRootSelectorConfig $ App $ LoadMarloweContext Query.ContractFound
  , getDefaultLogConfig getRootSelectorConfig $ App $ LoadMarloweContext Query.ExtractCreationFailed
  , getDefaultLogConfig getRootSelectorConfig $ App $ LoadMarloweContext Query.ExtractMarloweTransactionFailed
  , getDefaultLogConfig getRootSelectorConfig $ App $ LoadMarloweContext $ Query.ContractTipFound MarloweV1
  , getDefaultLogConfig getRootSelectorConfig $ ConfigWatcher ReloadConfig
  ]
