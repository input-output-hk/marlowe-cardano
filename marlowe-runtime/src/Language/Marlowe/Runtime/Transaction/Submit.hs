{-# LANGUAGE RankNTypes #-}
module Language.Marlowe.Runtime.Transaction.Submit
  where

import Cardano.Api (ScriptDataSupportedInEra, Tx)
import qualified Cardano.Api as C
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Transaction.Api (SubmitError(..), SubmitStatus(..))
import Network.Protocol.Job.Client (JobClient, liftCommand)

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError

data SubmitJobDependencies = SubmitJobDependencies
  { connectToChainSeek :: forall a. RuntimeChainSeekClient IO a -> IO a
  , runChainSyncJobClient :: forall a. JobClient ChainSyncCommand IO a -> IO a
  }

data SubmitJob = SubmitJob
  { submitJobStatus :: STM SubmitJobStatus
  , runSubmitJob :: IO ()
  }

mkSubmitJob
  :: SubmitJobDependencies
  -> forall era
   . ScriptDataSupportedInEra era
  -> Tx era
  -> STM SubmitJob
mkSubmitJob deps era tx = do
  statusVar <- newTVar $ Running Submitting
  pure $ SubmitJob (readTVar statusVar) $ doSubmit deps (atomically . writeTVar statusVar) era tx

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
          , recvMsgHandshakeConfirmed = pure clientIdle
          }
        clientIdle = SendMsgQueryNext (FindTx txId True) clientNext (pure clientNext)
        clientNext = ClientStNext
          { recvMsgQueryRejected = \err _ ->
              error $ "chainseekd rejected query: " <> show err
          , recvMsgRollBackward = \_ _ -> pure clientIdle
          , recvMsgRollForward = \_ point _ -> case point of
              Genesis -> error "chainseekd rolled forward to genesis"
              At block -> pure $ SendMsgDone block
          }
