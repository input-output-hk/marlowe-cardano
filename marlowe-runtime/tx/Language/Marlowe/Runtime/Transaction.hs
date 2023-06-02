{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction
  where

import Cardano.Api (Tx)
import Cardano.Api.Byron (BabbageEra)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Event.Class (MonadInjectEvent)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Chain
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import Language.Marlowe.Runtime.Transaction.Server
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob)
import Network.Protocol.Connection (SomeClientConnectorTraced, SomeConnectionSourceTraced)
import Network.Protocol.Driver.Trace (HasSpanContext)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Client (QueryClient)
import UnliftIO (MonadUnliftIO)

data TransactionDependencies r s m = TransactionDependencies
  { chainSyncConnector :: SomeClientConnectorTraced RuntimeChainSeekClient r s m
  , connectionSource :: SomeConnectionSourceTraced (JobServer MarloweTxCommand) r s m
  , mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , chainSyncQueryConnector :: SomeClientConnectorTraced (QueryClient ChainSyncQuery) r s m
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
