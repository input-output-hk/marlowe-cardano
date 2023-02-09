{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Submit
  where

import Cardano.Api (BabbageEra, ScriptDataSupportedInEra(..), Tx)
import qualified Cardano.Api as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Data.Functor (($>))
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Transaction.Api (SubmitError(..), SubmitStatus(..))
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Job.Client (JobClient, liftCommand)

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError

data SubmitJobDependencies = SubmitJobDependencies
  { connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  , runChainSyncJobClient :: forall a. JobClient ChainSyncCommand IO a -> IO a
  , submitConfirmationBlocks :: BlockNo
  }

data SubmitJob = SubmitJob
  { submitJobStatus :: STM SubmitJobStatus
  , runSubmitJob :: IO ()
  }

mkSubmitJob
  :: SubmitJobDependencies
  -> Tx BabbageEra
  -> STM SubmitJob
mkSubmitJob deps tx = do
  statusVar <- newTVar $ Running Submitting
  pure $ SubmitJob (readTVar statusVar) $ doSubmit deps (atomically . writeTVar statusVar) ScriptDataInBabbageEra tx

doSubmit
  :: SubmitJobDependencies
  -> (SubmitJobStatus -> IO ())
  -> ScriptDataSupportedInEra era
  -> Tx era
  -> IO ()
doSubmit SubmitJobDependencies{..} tellStatus era tx= do
  result <- runChainSyncJobClient $ liftCommand $ SubmitTx era tx
  case result of
    Left msg -> tellStatus $ Failed $ SubmitFailed msg
    Right _ -> do
      tellStatus $ Running Accepted
      tellStatus . either Succeeded (const $ Failed TxDiscarded) =<< race waitForTx timeout
  where
    txId = TxId $ C.serialiseToRawBytes $ C.getTxId $ C.getTxBody tx

    timeout :: IO ()
    timeout = threadDelay $ 10 * 60 * 1_000_000 -- 10 minutes in microseconds

    waitForTx :: IO BlockHeader
    waitForTx = connectToChainSeek client
      where
        client = ChainSeekClient $ pure clientInit

        clientInit = SendMsgRequestHandshake moveSchema ClientStHandshake
          { recvMsgHandshakeRejected = \_ ->
              error "chainseekd schema version mismatch"
          , recvMsgHandshakeConfirmed = pure clientIdleAwaitConfirmation
          }

        clientIdleAwaitConfirmation = SendMsgQueryNext (FindTx txId True) clientNextAwaitConfirmation

        clientNextAwaitConfirmation = ClientStNext
          { recvMsgQueryRejected = \err _ ->
              error $ "chainseekd rejected query: " <> show err
          , recvMsgRollBackward = \_ _ -> pure clientIdleAwaitConfirmation
          , recvMsgRollForward = \_ point tip -> case (point, tip) of
              (At block, At tipBlock) -> pure $ clientIdleAwaitMaturity block tipBlock
              _ -> error "chainseekd rolled forward to genesis"
          , recvMsgWait = threadDelay 100_000 $> SendMsgPoll clientNextAwaitConfirmation
          }

        clientIdleAwaitMaturity block tipBlock
          | blockNo tipBlock - blockNo block >= submitConfirmationBlocks = SendMsgDone block
          | otherwise = SendMsgQueryNext AdvanceToTip $ clientNextAwaitMaturity block

        clientNextAwaitMaturity block = ClientStNext
          { recvMsgQueryRejected = absurd
          , recvMsgRollBackward = \rollbackPoint _ -> case rollbackPoint of
              Genesis -> pure clientIdleAwaitConfirmation
              At rollbackBlock
                | rollbackBlock < block -> pure clientIdleAwaitConfirmation
                | otherwise -> pure $ clientIdleAwaitMaturity block rollbackBlock
          , recvMsgRollForward = \_ _ tip' -> case tip' of
              Genesis -> error "chainseekd rolled forward to genesis"
              At tipBlock' -> pure $ clientIdleAwaitMaturity block tipBlock'
          , recvMsgWait = threadDelay 100_000 $> SendMsgPoll (clientNextAwaitMaturity block)
          }
