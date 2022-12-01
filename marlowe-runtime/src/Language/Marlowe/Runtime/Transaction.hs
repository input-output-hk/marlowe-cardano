{-# LANGUAGE Arrows #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction
  where

import Cardano.Api (Tx)
import Cardano.Api.Byron (BabbageEra)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM)
import Data.Void
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery, RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Transaction.Chain
import Language.Marlowe.Runtime.Transaction.Query (LoadMarloweContext, LoadWalletContext)
import Language.Marlowe.Runtime.Transaction.Server
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob)
import Network.Protocol.Driver (RunClient)

data TransactionDependencies = TransactionDependencies
  { connectToChainSeek :: RunClient IO RuntimeChainSeekClient
  , acceptRunTransactionServer :: WorkerM (RunTransactionServer WorkerM)
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , logAction :: AppLogAction
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
  }

transaction :: Component IO TransactionDependencies ()
transaction = proc TransactionDependencies{..} -> do
  getTip <- transactionChainClient -< TransactionChainClientDependencies{..}
  transactionServer -< TransactionServerDependencies{..}
