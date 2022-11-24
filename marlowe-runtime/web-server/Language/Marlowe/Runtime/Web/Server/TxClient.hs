{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
module Language.Marlowe.Runtime.Web.Server.TxClient
  where

import Cardano.Api (BabbageEra)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar)
import Data.Foldable (for_)
import qualified Data.Map as Map
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, StakeCredential, TransactionMetadata)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, MarloweVersion)
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), CreateError, MarloweTxCommand(..), RoleTokensConfig, WalletAddresses)
import Network.Protocol.Driver (RunClient)
import Network.Protocol.Job.Client

newtype TxClientDependencies = TxClientDependencies
  { runTxJobClient :: RunClient IO (JobClient MarloweTxCommand)
  }

type CreateContract m
   = forall v
   . Maybe StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> TransactionMetadata
  -> Lovelace
  -> Contract v
  -> m (Either (CreateError v) (ContractCreated BabbageEra v))

data TempContract where
  Created :: ContractCreated BabbageEra v -> TempContract

-- | Public API of the TxClient
data TxClient = TxClient
  { createContract :: CreateContract IO -- ^ Load contract headers from the indexer.
  , lookupTempContract :: ContractId -> STM (Maybe TempContract) -- ^ Lookup contract headers that have been built or are being submitted
  , getTempContracts :: STM [TempContract]
  }

txClient :: Component IO TxClientDependencies TxClient
txClient = component \TxClientDependencies{..} -> do
  tempContracts <- newTVar mempty
  pure $ pure TxClient
    { createContract = \stakeCredential version addresses roles metadata minUTxODeposit contract -> do
        response <- runTxJobClient
          $ liftCommand
          $ Create stakeCredential version addresses roles metadata minUTxODeposit contract
        for_ response \creation -> atomically
          $ modifyTVar tempContracts
          $ Map.insert (contractId creation)
          $ Created creation
        pure response
    , lookupTempContract = \contractId -> Map.lookup contractId <$> readTVar tempContracts
    , getTempContracts = fmap snd . Map.toAscList <$> readTVar tempContracts
    }
