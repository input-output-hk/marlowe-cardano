{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.Query.Server where

import Cardano.Api (CardanoMode, EraHistory (..), SlotNo (SlotNo), SystemStart (getSystemStart))
import Control.Monad.IO.Class (MonadIO)
import Data.Time (UTCTime)
import Data.Version (Version)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader (..),
  ChainPoint,
  ChainSyncQuery (..),
  SlotNo (unSlotNo),
  TxId,
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Query.Client (QueryClient, request)
import Network.Protocol.Query.Server
import Ouroboros.Consensus.BlockchainTime (fromRelativeTime)
import Ouroboros.Consensus.HardFork.History (interpretQuery, slotToWallclock)
import UnliftIO (MonadUnliftIO, concurrently, throwIO)

type MarloweQueryServer = QueryServer MarloweSyncRequest

marloweQueryServer
  :: forall m
   . (MonadUnliftIO m)
  => Version
  -> Connector (QueryClient ChainSyncQuery) m
  -> m ChainPoint
  -> (ContractFilter -> Range ContractId -> m (Maybe (Page ContractId ContractHeader)))
  -> (ContractId -> m (Maybe SomeContractState))
  -> (TxId -> m (Maybe SomeTransaction))
  -> (ContractId -> m (Maybe SomeTransactions))
  -> (TxId -> m (Maybe Withdrawal))
  -> (WithdrawalFilter -> Range TxId -> m (Maybe (Page TxId Withdrawal)))
  -> MarloweQueryServer m ()
marloweQueryServer runtimeVersion chainQueryConnector getRuntimeTip getContractHeaders getContractState getTransaction getTransactions getWithdrawal getWithdrawals =
  respond concurrently \case
    ReqContractHeaders cFilter range -> getContractHeaders cFilter range
    ReqContractState contractId -> getContractState contractId
    ReqTransaction txId -> getTransaction txId
    ReqTransactions contractId -> getTransactions contractId
    ReqWithdrawal txId -> getWithdrawal txId
    ReqWithdrawals wFilter range -> getWithdrawals wFilter range
    ReqStatus -> do
      ((nodeTip, runtimeChainTip, systemStart, history, networkId), runtimeTip) <-
        concurrently
          ( runConnector chainQueryConnector do
              nodeTip <- request GetNodeTip
              runtimeChainTip <- request GetTip
              systemStart <- request GetSystemStart
              history <- request GetEraHistory
              networkId <- request GetNetworkId
              pure (nodeTip, runtimeChainTip, systemStart, history, networkId)
          )
          getRuntimeTip
      nodeTipUTC <- slotToUTCTime systemStart history nodeTip
      runtimeChainTipUTC <- slotToUTCTime systemStart history runtimeChainTip
      runtimeTipUTC <- slotToUTCTime systemStart history runtimeTip
      pure RuntimeStatus{..}

slotToUTCTime :: (MonadIO m) => SystemStart -> EraHistory CardanoMode -> ChainPoint -> m UTCTime
slotToUTCTime systemStart (EraHistory _ interpreter) = \case
  Genesis -> pure $ getSystemStart systemStart
  At BlockHeader{..} -> case interpretQuery interpreter $ slotToWallclock $ SlotNo $ unSlotNo slotNo of
    Left err -> throwIO err
    Right (relative, _) -> pure $ fromRelativeTime systemStart relative
