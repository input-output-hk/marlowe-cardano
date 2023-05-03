{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Logging
  ( RootSelector(..)
  , defaultRootSelectorLogConfig
  , getRootSelectorConfig
  ) where

import Control.Monad.Event.Class (Inject(..))
import Data.Foldable (fold)
import Data.Map (Map)
import Data.Text (Text)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Transaction (getTransactionSererSelectorConfig)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Server
import Network.Protocol.Connection (ConnectorSelector, getConnectorSelectorConfig, getDefaultConnectorLogConfig)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event (idInjectSelector, injectSelector)
import Observe.Event.Component
  ( ConfigWatcherSelector(..)
  , FieldConfig(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SelectorLogConfig
  , SomeJSON(SomeJSON)
  , absurdFieldConfig
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
  LoadWalletContext :: Q.LoadWalletContextSelector f -> RootSelector f
  LoadMarloweContext :: Q.LoadMarloweContextSelector f -> RootSelector f

instance Inject RootSelector RootSelector where
  inject = idInjectSelector

instance Inject Q.LoadWalletContextSelector RootSelector where
  inject = injectSelector LoadWalletContext

instance Inject Q.LoadMarloweContextSelector RootSelector where
  inject = injectSelector LoadMarloweContext

instance Inject TransactionServerSelector RootSelector where
  inject = injectSelector App

getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-sync" $ getConnectorSelectorConfig False False sel
  ChainSyncJobClient sel -> prependKey "chain-sync-job" $ getConnectorSelectorConfig False False sel
  ChainSyncQueryClient sel -> prependKey "chain-sync-query" $ getConnectorSelectorConfig False False sel
  Server sel -> prependKey "server" $ getConnectorSelectorConfig False True sel
  App sel -> getTransactionSererSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True
  LoadWalletContext Q.LoadWalletContext -> SelectorConfig "load-wallet-context" True FieldConfig
    { fieldKey = \case
        Q.ForAddresses _ -> "for-addresses"
        Q.WalletContextLoaded _ -> "wallet-context"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \case
        Q.ForAddresses addresses -> SomeJSON addresses
        Q.WalletContextLoaded context -> SomeJSON context
    }
  LoadMarloweContext sel -> prependKey "load-marlowe-context" $ getLoadMarloweContextSelectorConfig sel

getLoadMarloweContextSelectorConfig :: GetSelectorConfig Q.LoadMarloweContextSelector
getLoadMarloweContextSelectorConfig = \case
  Q.ExtractCreationFailed -> SelectorConfig "extract-creation-failed" True $ singletonFieldConfig "error" True
  Q.ExtractMarloweTransactionFailed -> SelectorConfig "extract-transaction-failed" True $ singletonFieldConfig "error" True
  Q.ContractNotFound -> SelectorConfig "contract-not-found" True absurdFieldConfig
  Q.ContractFound -> SelectorConfig "contract-found" True FieldConfig
    { fieldKey = \case
        Q.ActualVersion _ -> "actual-version"
        Q.MarloweScriptAddress _ -> "marlowe-script-address"
        Q.PayoutScriptHash _ -> "payout-script-hash"
        Q.ContractUTxO _ -> "contract-utxo"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \case
        Q.ActualVersion version -> SomeJSON version
        Q.MarloweScriptAddress address -> SomeJSON address
        Q.PayoutScriptHash hash -> SomeJSON hash
        Q.ContractUTxO utxo -> SomeJSON utxo
    }
  Q.ContractTipFound version -> SelectorConfig "contract-tip-found" True FieldConfig
    { fieldKey = const "context"
    , fieldDefaultEnabled = const True
    , toSomeJSON = \ctx -> case version of
        MarloweV1 -> SomeJSON ctx
    }

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
  , getDefaultLogConfig getRootSelectorConfig $ LoadWalletContext Q.LoadWalletContext
  , getDefaultLogConfig getRootSelectorConfig $ LoadMarloweContext Q.ContractNotFound
  , getDefaultLogConfig getRootSelectorConfig $ LoadMarloweContext Q.ContractFound
  , getDefaultLogConfig getRootSelectorConfig $ LoadMarloweContext Q.ExtractCreationFailed
  , getDefaultLogConfig getRootSelectorConfig $ LoadMarloweContext Q.ExtractMarloweTransactionFailed
  , getDefaultLogConfig getRootSelectorConfig $ LoadMarloweContext $ Q.ContractTipFound MarloweV1
  , getDefaultLogConfig getRootSelectorConfig $ ConfigWatcher ReloadConfig
  ]
