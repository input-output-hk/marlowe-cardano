{-# LANGUAGE RankNTypes #-}
module Language.Marlowe.Runtime.Transaction.Submit
  where

import Cardano.Api (BabbageEra, ScriptDataSupportedInEra(..), Tx)
import qualified Cardano.Api as C
import Colog (logError, logWarning)
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Language.Marlowe.Runtime.ChainSync.Api
import qualified Language.Marlowe.Runtime.ChainSync.Api as ChainSync
import Language.Marlowe.Runtime.Logging.Colog.LogIO (LogIO, raceLogIO)
import Language.Marlowe.Runtime.Transaction.Api (SubmitError(..), SubmitStatus(..))
import Network.Protocol.Job.Client (JobClient, liftCommand)

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError

data SubmitJobDependencies = SubmitJobDependencies
  { connectToChainSeek :: forall a. RuntimeChainSeekClient LogIO a -> LogIO a
  , runChainSyncJobClient :: forall a. JobClient ChainSyncCommand LogIO a -> LogIO a
  }

data SubmitJob = SubmitJob
  { submitJobStatus :: STM SubmitJobStatus
  , runSubmitJob :: LogIO ()
  }

mkSubmitJob
  :: SubmitJobDependencies
  -> Tx BabbageEra
  -> STM SubmitJob
mkSubmitJob deps tx = do
  statusVar <- newTVar $ Running Submitting
  pure $ SubmitJob (readTVar statusVar) $ doSubmit deps (liftIO . atomically . writeTVar statusVar) ScriptDataInBabbageEra tx

doSubmit
  :: HasCallStack
  => SubmitJobDependencies
  -> (SubmitJobStatus -> LogIO ())
  -> ScriptDataSupportedInEra era
  -> Tx era
  -> LogIO ()
doSubmit SubmitJobDependencies{..} tellStatus era tx= do
  result <- runChainSyncJobClient $ liftCommand ChainSync.commandSchema $ SubmitTx era tx
  case result of
    Left (Left _) ->
      tellStatus $ Failed HandshakeFailed
    Left (Right msg) ->
      tellStatus $ Failed $ SubmitFailed msg
    Right _ -> do
      tellStatus $ Running Accepted
      status <- either id id <$> raceLogIO waitForTx timeout
      tellStatus status
  where
    txId = TxId $ C.serialiseToRawBytes $ C.getTxId $ C.getTxBody tx

    timeout :: LogIO SubmitJobStatus
    timeout = do
      liftIO $ threadDelay $ 10 * 60 * 1_000_000 -- 10 minutes in microseconds
      logWarning "Submit timeout reached."
      pure $ Failed TxDiscarded

    waitForTx :: LogIO SubmitJobStatus
    waitForTx = connectToChainSeek client
      where
        client = ChainSeekClient $ pure clientInit
        clientInit = SendMsgRequestHandshake moveSchema ClientStHandshake
          { recvMsgHandshakeRejected = \versions -> do
              logError . T.pack $
                  "ChainSeek handshake failed. Requested schema version "
                  <> show moveSchema
                  <> ", requires "
                  <> show versions
                  <> "."
              pure $ Failed HandshakeFailed

          , recvMsgHandshakeConfirmed = pure clientIdle
          }
        clientIdle = SendMsgQueryNext (FindTx txId True) clientNext (pure clientNext)
        clientNext = ClientStNext
          { recvMsgQueryRejected = \err _ -> do
              logError . T.pack . mappend "Chainseekd rejected query: " $ show err
              pure $ SendMsgDone $ Failed SubmitRejected
          , recvMsgRollBackward = \_ _ -> pure clientIdle
          , recvMsgRollForward = \_ point _ -> case point of
              Genesis -> do
                let
                  msg = "Chainseekd rolled forward to genesis"
                logError . T.pack $ msg
                pure $ SendMsgDone $ Failed (SubmitFailed  msg)
              At block -> pure $ SendMsgDone $ Succeeded block
          }


