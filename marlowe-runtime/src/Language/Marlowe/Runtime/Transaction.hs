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
import Observe.Event (EventBackend)

data TransactionDependencies r = TransactionDependencies
  { connectToChainSeek :: RunClient IO RuntimeChainSeekClient
  , acceptRunTransactionServer :: IO (RunTransactionServer IO)
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext r
  , loadMarloweContext :: LoadMarloweContext r
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
  , eventBackend :: EventBackend IO r TransactionServerSelector
  }

transaction :: Component IO (TransactionDependencies r) ()
transaction = proc TransactionDependencies{..} -> do
  getTip <- transactionChainClient -< TransactionChainClientDependencies{..}
  transactionServer -< TransactionServerDependencies{..}
