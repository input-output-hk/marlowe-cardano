{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Web.Server.TxClient
  where

import Cardano.Api (BabbageEra, getTxId)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar)
import Control.Monad ((<=<))
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, StakeCredential, TransactionMetadata, TxId)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, MarloweVersion, MarloweVersionTag, Redeemer)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , ContractCreated(..)
  , CreateError
  , InputsApplied(..)
  , MarloweTxCommand(..)
  , RoleTokensConfig
  , WalletAddresses
  )
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

type ApplyInputs m
   = forall v
   . MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Redeemer v
  -> m (Either (ApplyInputsError v) (InputsApplied BabbageEra v))

data TempTx (tx :: * -> MarloweVersionTag -> *) where
  Created :: tx BabbageEra v -> TempTx tx

-- | Public API of the TxClient
data TxClient = TxClient
  { createContract :: CreateContract IO
  , applyInputs :: ApplyInputs IO
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreated))
  , getTempContracts :: STM [TempTx ContractCreated]
  , lookupTempTransaction :: ContractId -> TxId -> STM (Maybe (TempTx InputsApplied))
  , getTempTransactions :: ContractId -> STM [TempTx InputsApplied]
  }

txClient :: Component IO TxClientDependencies TxClient
txClient = component \TxClientDependencies{..} -> do
  tempContracts <- newTVar mempty
  tempTransactions <- newTVar mempty
  pure $ pure TxClient
    { createContract = \stakeCredential version addresses roles metadata minUTxODeposit contract -> do
        response <- runTxJobClient
          $ liftCommand
          $ Create stakeCredential version addresses roles metadata minUTxODeposit contract
        for_ response \creation@ContractCreated{contractId} -> atomically
          $ modifyTVar tempContracts
          $ Map.insert contractId
          $ Created creation
        pure response
    , applyInputs = \version addresses contractId invalidBefore invalidHereafter inputs -> do
        response <- runTxJobClient
          $ liftCommand
          $ ApplyInputs version addresses contractId invalidBefore invalidHereafter inputs
        for_ response \application@InputsApplied{txBody} -> do
          let txId = fromCardanoTxId $ getTxId txBody
          let tempTx = Created application
          atomically
            $ modifyTVar tempTransactions
            $ Map.alter (Just . maybe (Map.singleton txId tempTx) (Map.insert txId tempTx)) contractId
        pure response
    , lookupTempContract = \contractId -> Map.lookup contractId <$> readTVar tempContracts
    , getTempContracts = fmap snd . Map.toAscList <$> readTVar tempContracts
    , lookupTempTransaction = \contractId txId -> (Map.lookup txId <=< Map.lookup contractId) <$> readTVar tempTransactions
    , getTempTransactions = \contractId -> fmap snd . foldMap Map.toList . Map.lookup contractId <$> readTVar tempTransactions
    }
