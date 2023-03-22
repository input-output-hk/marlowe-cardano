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
import Network.Protocol.Connection (SomeClientConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Job.Client (JobClient, liftCommand)

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError

data SubmitJobDependencies = SubmitJobDependencies
  { chainSyncConnector :: SomeClientConnector RuntimeChainSeekClient IO
  , chainSyncJobConnector :: SomeClientConnector (JobClient ChainSyncCommand) IO
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
  result <- runSomeConnector chainSyncJobConnector $ liftCommand $ SubmitTx era tx
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
    waitForTx = runSomeConnector chainSyncConnector client
      where
        client = ChainSeekClient $ pure clientIdleAwaitConfirmation

        clientIdleAwaitConfirmation = SendMsgQueryNext (FindTx txId True) clientNextAwaitConfirmation

        clientNextAwaitConfirmation = ClientStNext
          { recvMsgQueryRejected = \err _ ->
              error $ "marlowe-chain-sync rejected query: " <> show err
          , recvMsgRollBackward = \_ _ -> pure clientIdleAwaitConfirmation
          , recvMsgRollForward = \_ point _ -> case point of
              Genesis -> error "marlowe-chain-sync rolled forward to genesis"
              At block -> pure $ clientIdleAwaitMaturity block
          , recvMsgWait = threadDelay 100_000 $> SendMsgPoll clientNextAwaitConfirmation
          }

        clientIdleAwaitMaturity confirmationBlock
          | submitConfirmationBlocks == 0 = SendMsgDone confirmationBlock
          | otherwise = SendMsgQueryNext (AdvanceBlocks $ fromIntegral submitConfirmationBlocks)
              $ clientNextAwaitMaturity confirmationBlock

        clientNextAwaitMaturity confirmationBlock = ClientStNext
          { recvMsgQueryRejected = absurd
          , recvMsgRollBackward = \_ _ -> pure clientIdleAwaitConfirmation
          , recvMsgRollForward = \_ _ _ -> pure $ SendMsgDone confirmationBlock
          , recvMsgWait = threadDelay 100_000 $> SendMsgPoll (clientNextAwaitMaturity confirmationBlock)
          }
