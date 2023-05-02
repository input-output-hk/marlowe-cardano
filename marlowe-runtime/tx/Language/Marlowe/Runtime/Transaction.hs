{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction
  where

import Cardano.Api (Tx)
import qualified Cardano.Api as C
import Cardano.Api.Byron (BabbageEra)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Event.Class (MonadInjectEvent)
import Data.Text (Text)
import Data.Void
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Chain
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import Language.Marlowe.Runtime.Transaction.Server
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob)
import Network.Protocol.Connection (SomeClientConnectorTraced, SomeConnectionSourceTraced)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Peer.Trace (HasSpanContext)
import Observe.Event.Component (FieldConfig(..), GetSelectorConfig, SelectorConfig(..), SomeJSON(..))
import UnliftIO (MonadUnliftIO)

data TransactionDependencies r s m = TransactionDependencies
  { chainSyncConnector :: SomeClientConnectorTraced RuntimeChainSeekClient r s m
  , connectionSource :: SomeConnectionSourceTraced (JobServer MarloweTxCommand) r s m
  , mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> m a
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  }

transaction
  :: (MonadUnliftIO m, MonadInjectEvent r TransactionServerSelector s m, HasSpanContext r)
  => Component m (TransactionDependencies r s m) Probes
transaction = proc TransactionDependencies{..} -> do
  (connected, getTip) <- transactionChainClient -< TransactionChainClientDependencies{..}
  transactionServer -< TransactionServerDependencies{..}
  returnA -< Probes
    { startup = pure True
    , liveness = atomically connected
    , readiness = atomically connected
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
