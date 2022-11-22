{-# LANGUAGE RankNTypes #-}
module Language.Marlowe.Runtime.Web.Server.TxClient
  where

import Cardano.Api (BabbageEra, TxBody)
import Control.Concurrent.STM (STM)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, StakeCredential, TransactionMetadata)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, MarloweVersion)
import Language.Marlowe.Runtime.Transaction.Api (CreateError, MarloweTxCommand(..), RoleTokensConfig, WalletAddresses)
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
  -> m (Either (CreateError v) (ContractId, TxBody BabbageEra))

-- | Public API of the TxClient
newtype TxClient = TxClient
  { createContract :: CreateContract IO -- ^ Load contract headers from the indexer.
  }

mkTxClient :: TxClientDependencies -> STM TxClient
mkTxClient TxClientDependencies{..} = pure TxClient
  { createContract = \stakeCredential version addresses roles metadata minUTxODeposit contract ->
      runTxJobClient
        $ liftCommand
        $ Create stakeCredential version addresses roles metadata minUTxODeposit contract
  }
