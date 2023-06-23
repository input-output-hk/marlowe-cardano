{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Submit where

import Cardano.Api (BabbageEra, ScriptDataSupportedInEra (..), Tx)
import qualified Cardano.Api as C
import Control.Concurrent.STM (STM, newTVar, readTVar, writeTVar)
import Data.Functor (($>))
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Transaction.Api (SubmitError (..), SubmitStatus (..))
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Job.Client (JobClient, liftCommand)
import UnliftIO (MonadUnliftIO, atomically, race)
import UnliftIO.Concurrent (threadDelay)

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError

data SubmitJobDependencies m = SubmitJobDependencies
  { chainSyncConnector :: Connector RuntimeChainSeekClient m
  , chainSyncJobConnector :: Connector (JobClient ChainSyncCommand) m
  , confirmationTimeout :: NominalDiffTime
  , pollingInterval :: NominalDiffTime
  , submitConfirmationBlocks :: BlockNo
  }

data SubmitJob m = SubmitJob
  { submitJobStatus :: STM SubmitJobStatus
  , runSubmitJob :: m ()
  }

mkSubmitJob
  :: (MonadUnliftIO m)
  => SubmitJobDependencies m
  -> Tx BabbageEra
  -> STM (SubmitJob m)
mkSubmitJob deps tx = do
  statusVar <- newTVar $ Running Submitting
  pure $ SubmitJob (readTVar statusVar) $ doSubmit deps (atomically . writeTVar statusVar) ScriptDataInBabbageEra tx

doSubmit
  :: forall m era
   . (MonadUnliftIO m)
  => SubmitJobDependencies m
  -> (SubmitJobStatus -> m ())
  -> ScriptDataSupportedInEra era
  -> Tx era
  -> m ()
doSubmit SubmitJobDependencies{..} tellStatus era tx = do
  result <- runConnector chainSyncJobConnector $ liftCommand $ SubmitTx era tx
  case result of
    Left msg -> tellStatus $ Failed $ SubmitFailed msg
    Right _ -> do
      tellStatus $ Running Accepted
      tellStatus . either Succeeded (const $ Failed TxDiscarded) =<< race waitForTx timeout
  where
    txId = TxId $ C.serialiseToRawBytes $ C.getTxId $ C.getTxBody tx

    timeout :: m ()
    timeout = threadDelay $ floor $ nominalDiffTimeToSeconds confirmationTimeout * 1_000_000

    pollingMicroSeconds = floor $ nominalDiffTimeToSeconds pollingInterval * 1_000_000

    waitForTx :: m BlockHeader
    waitForTx = runConnector chainSyncConnector client
      where
        client = ChainSeekClient $ pure clientIdleAwaitConfirmation

        clientIdleAwaitConfirmation = SendMsgQueryNext (FindTx txId True) clientNextAwaitConfirmation

        clientNextAwaitConfirmation =
          ClientStNext
            { recvMsgQueryRejected = \err _ ->
                error $ "marlowe-chain-sync rejected query: " <> show err
            , recvMsgRollBackward = \_ _ -> pure clientIdleAwaitConfirmation
            , recvMsgRollForward = \_ point _ -> case point of
                Genesis -> error "marlowe-chain-sync rolled forward to genesis"
                At block -> pure $ clientIdleAwaitMaturity block
            , recvMsgWait = threadDelay pollingMicroSeconds $> SendMsgPoll clientNextAwaitConfirmation
            }

        clientIdleAwaitMaturity confirmationBlock
          | submitConfirmationBlocks == 0 = SendMsgDone confirmationBlock
          | otherwise =
              SendMsgQueryNext (AdvanceBlocks $ fromIntegral submitConfirmationBlocks) $
                clientNextAwaitMaturity confirmationBlock

        clientNextAwaitMaturity confirmationBlock =
          ClientStNext
            { recvMsgQueryRejected = absurd
            , recvMsgRollBackward = \_ _ -> pure clientIdleAwaitConfirmation
            , recvMsgRollForward = \_ _ _ -> pure $ SendMsgDone confirmationBlock
            , recvMsgWait = threadDelay pollingMicroSeconds $> SendMsgPoll (clientNextAwaitMaturity confirmationBlock)
            }
