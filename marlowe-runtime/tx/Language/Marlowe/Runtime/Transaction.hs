{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction where

import Cardano.Api (Tx)
import Cardano.Api.Byron (BabbageEra)
import Colog (Message, WithLog)
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Concurrent.STM (STM, atomically)
import Control.Monad.Event.Class (MonadInjectEvent)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(..))
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Chain
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import Language.Marlowe.Runtime.Transaction.Server
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob)
import Network.Protocol.Connection (ConnectionSource, Connector)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Client (QueryClient)
import UnliftIO (MonadUnliftIO)

data TransactionDependencies m = TransactionDependencies
  { chainSyncConnector :: Connector RuntimeChainSeekClient m
  , connectionSource :: ConnectionSource (JobServer MarloweTxCommand) m
  , mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , contractQueryConnector :: Connector (QueryClient ContractRequest) m
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  }

transaction
  :: (MonadUnliftIO m, MonadInjectEvent r TransactionServerSelector s m, WithLog env Message m)
  => Component m (TransactionDependencies m) Probes
transaction = proc TransactionDependencies{..} -> do
  (connected, getTip) <- transactionChainClient -< TransactionChainClientDependencies{..}
  transactionServer -< TransactionServerDependencies{..}
  returnA -< Probes
    { startup = pure True
    , liveness = atomically connected
    , readiness = atomically connected
    }
