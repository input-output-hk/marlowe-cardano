{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Submit where

import Cardano.Api (BabbageEra, ScriptDataSupportedInEra (..), Tx)
import qualified Cardano.Api as C
import Colog (Message, WithLog, logWarning)
import Control.Concurrent.STM (STM, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException)
import Data.Functor (($>))
import Data.String (fromString)
import Data.Time (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.Transaction.Api (SubmitError (..), SubmitStatus (..))
import Network.Protocol.ChainSeek.Client
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Job.Client (JobClient, liftCommand)
import UnliftIO (MonadUnliftIO, atomically, catch, race)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Retry (
  defaultLogMsg,
  fullJitterBackoff,
  limitRetriesByDelay,
  logRetries,
  recovering,
  skipAsyncExceptions,
 )

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError
  | Crashed SomeException

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
  :: (WithLog env Message m, MonadUnliftIO m)
  => SubmitJobDependencies m
  -> Tx BabbageEra
  -> STM (SubmitJob m)
mkSubmitJob deps tx = do
  statusVar <- newTVar $ Running Submitting
  pure $ SubmitJob (readTVar statusVar) do
    catch
      (doSubmit deps (atomically . writeTVar statusVar) ScriptDataInBabbageEra tx)
      (atomically . writeTVar statusVar . Crashed)

doSubmit
  :: forall env m era
   . (WithLog env Message m, MonadUnliftIO m)
  => SubmitJobDependencies m
  -> (SubmitJobStatus -> m ())
  -> ScriptDataSupportedInEra era
  -> Tx era
  -> m ()
doSubmit SubmitJobDependencies{..} tellStatus era tx = recovering retryPolicy handlers $ const do
  result <- runConnector chainSyncJobConnector $ liftCommand $ SubmitTx era tx
  case result of
    Left msg -> tellStatus $ Failed $ SubmitFailed msg
    Right _ -> do
      tellStatus $ Running Accepted
      tellStatus . either Succeeded (const $ Failed TxDiscarded) =<< race waitForTx timeout
  where
    -- Determines whether or not to retry based on the type of exception thrown.
    handlers =
      -- We do not want to catch async exceptions
      mappend skipAsyncExceptions $
        pure $
          -- Catch any other kind of exception and log the retry message as a warning.
          logRetries
            (const $ pure True)
            ((fmap . fmap) (logWarning . fromString . (<> ("[submit tx: " <> show txId <> "]"))) . defaultLogMsg @SomeException)
    -- Abort submit if our reconnect delay exceeds one minute.
    maxRetrySeconds = 60
    -- Use full-jitter backoff with a base delay of 100 ms, aborting if delay exceeds one minute.
    retryPolicy = limitRetriesByDelay (maxRetrySeconds * 1_000_000 + 1) $ fullJitterBackoff 100_000
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
