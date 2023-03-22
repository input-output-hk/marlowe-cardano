{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction
  where

import Cardano.Api (Tx)
import qualified Cardano.Api as C
import Cardano.Api.Byron (BabbageEra)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (STM, atomically)
import Data.Text (Text)
import Data.Void
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Chain
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import qualified Language.Marlowe.Runtime.Transaction.Query as Q
import Language.Marlowe.Runtime.Transaction.Server
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob)
import Network.Protocol.Connection (SomeClientConnector, SomeConnectionSource)
import Network.Protocol.Job.Server (JobServer)
import Observe.Event (EventBackend)
import Observe.Event.Component
  ( FieldConfig(..)
  , GetSelectorConfig
  , SelectorConfig(..)
  , SomeJSON(..)
  , absurdFieldConfig
  , prependKey
  , singletonFieldConfig
  )

data TransactionDependencies r = TransactionDependencies
  { chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient IO
  , connectionSource :: SomeConnectionSource (JobServer MarloweTxCommand) IO
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext r
  , loadMarloweContext :: LoadMarloweContext r
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
  , eventBackend :: EventBackend IO r TransactionServerSelector
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  , httpPort :: Int
  }

transaction :: Component IO (TransactionDependencies r) ()
transaction = proc TransactionDependencies{..} -> do
  (connected, getTip) <- transactionChainClient -< TransactionChainClientDependencies{..}
  transactionServer -< TransactionServerDependencies{..}
  probeServer -< ProbeServerDependencies
    { probes = Probes
        { startup = pure True
        , liveness = atomically connected
        , readiness = atomically connected
        }
    , port = httpPort
    }

getTransactionSererSelectorConfig :: GetSelectorConfig TransactionServerSelector
getTransactionSererSelectorConfig = \case
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

buildTxFieldConfig :: FieldConfig BuildTxField
buildTxFieldConfig = FieldConfig
  { fieldKey = \case
      Constraints _ _ -> "constraints"
      ResultingTxBody _ -> "tx-body"
  , fieldDefaultEnabled = const True
  , toSomeJSON = \case
      Constraints MarloweV1 constraints -> SomeJSON $ show constraints
      ResultingTxBody txBody -> SomeJSON $ show txBody
  }
