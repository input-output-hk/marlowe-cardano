{-# LANGUAGE GADTs #-}

module Logging
  ( RootSelector(..)
  , getRootSelectorConfig
  ) where

import qualified Cardano.Api as C
import Data.Text (Text)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeek)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Server
import Network.Protocol.Driver
  ( AcceptSocketDriverSelector
  , ConnectSocketDriverSelector
  , SocketDriverConfigOptions(..)
  , getAcceptSocketDriverSelectorConfig
  , getConnectSocketDriverSelectorConfig
  )
import Network.Protocol.Job.Types (Job)
import Network.Protocol.Query.Types (Query)
import Observe.Event.Component
  ( ConfigWatcherSelector(ReloadConfig)
  , FieldConfig(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SomeJSON(..)
  , absurdFieldConfig
  , prependKey
  , singletonFieldConfig
  )

data RootSelector f where
  ChainSeekClient :: ConnectSocketDriverSelector RuntimeChainSeek f -> RootSelector f
  ChainSyncJobClient :: ConnectSocketDriverSelector (Job ChainSyncCommand) f -> RootSelector f
  ChainSyncQueryClient :: ConnectSocketDriverSelector (Query ChainSyncQuery) f -> RootSelector f
  HistoryClient :: ConnectSocketDriverSelector MarloweSync f -> RootSelector f
  Server :: AcceptSocketDriverSelector (Job MarloweTxCommand) f -> RootSelector f
  App :: TransactionServerSelector f -> RootSelector f
  ConfigWatcher :: ConfigWatcherSelector f -> RootSelector f

-- TODO automate this boilerplate with Template Haskell
getRootSelectorConfig :: GetSelectorConfig RootSelector
getRootSelectorConfig = \case
  ChainSeekClient sel -> prependKey "chain-seek" $ getConnectSocketDriverSelectorConfig chainSeekConfig sel
  ChainSyncJobClient sel -> prependKey "chain-sync-job" $ getConnectSocketDriverSelectorConfig chainSyncJobConfig sel
  ChainSyncQueryClient sel -> prependKey "chain-sync-query" $ getConnectSocketDriverSelectorConfig chainSyncQueryConfig sel
  HistoryClient sel -> prependKey "history" $ getConnectSocketDriverSelectorConfig historyClientConfig sel
  Server sel -> prependKey "server" $ getAcceptSocketDriverSelectorConfig jobConfig sel
  App sel -> getAppSelectorConfig sel
  ConfigWatcher ReloadConfig -> SelectorConfig "reload-log-config" True
    $ singletonFieldConfig "config" True

getAppSelectorConfig :: GetSelectorConfig TransactionServerSelector
getAppSelectorConfig = \case
  Exec -> SelectorConfig "exec" True FieldConfig
    { fieldKey = \case
        SystemStart _ -> "system-start"
        EraHistory _ -> "era-history"
        ProtocolParameters _ -> "protocol-parameters"
        NetworkId _ -> "network-id"
    , fieldDefaultEnabled = const False
    , toSomeJSON = \case
        SystemStart (C.SystemStart ss) -> SomeJSON ss
        EraHistory _ -> SomeJSON @Text "<era history>"
        ProtocolParameters pp -> SomeJSON pp
        NetworkId C.Mainnet -> SomeJSON @Text "mainnet"
        NetworkId (C.Testnet (C.NetworkMagic m)) -> SomeJSON m
    }
  ExecCreate -> SelectorConfig "exec-create" True buildTxFieldConfig
  ExecApplyInputs -> SelectorConfig "exec-apply-inputs" True buildTxFieldConfig
  ExecWithdraw -> SelectorConfig "exec-withdraw" True buildTxFieldConfig
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

buildTxFieldConfig :: FieldConfig BuildTxField
buildTxFieldConfig = error "not implemented"

chainSeekConfig :: SocketDriverConfigOptions
chainSeekConfig = SocketDriverConfigOptions
  { enableConnected = True
  , enableDisconnected = True
  , enableServerDriverEvent = False
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

jobConfig :: SocketDriverConfigOptions
jobConfig = SocketDriverConfigOptions
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
