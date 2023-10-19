{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer where

import Cardano.Api (CardanoEra (..), CardanoMode, ScriptDataSupportedInEra (..), Tx, TxValidationErrorInMode)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand (..), Command (..), JobError (..), JobResult (..))
import Network.Protocol.Connection (ServerSource (..))
import Network.Protocol.Job.Server
import Network.Protocol.Job.Types (Job (..))
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult (..))
import UnliftIO (MonadUnliftIO)

newtype ChainSyncJobServerDependencies m = ChainSyncJobServerDependencies
  { submitTxToNodeLocal
      :: forall era
       . CardanoEra era
      -> Tx era
      -> m (SubmitResult (TxValidationErrorInMode CardanoMode))
  }

chainSyncJobServer
  :: forall m
   . (MonadUnliftIO m)
  => ChainSyncJobServerDependencies m
  -> ServerSource (JobServerT ChainSyncCommand 'StInit 'StDone) m ()
chainSyncJobServer ChainSyncJobServerDependencies{..} = ServerSource $ pure server
  where
    server :: JobServerT ChainSyncCommand 'StInit 'StDone m ()
    server = liftCommandHandler $ flip either (\case {}) \case
      CmdSubmitTx era tx ->
        ((),) <$> do
          result <-
            submitTxToNodeLocal
              case era of
                ScriptDataInAlonzoEra -> AlonzoEra
                ScriptDataInBabbageEra -> BabbageEra
                ScriptDataInConwayEra -> ConwayEra
              tx
          pure case result of
            SubmitFail err -> Left $ ErrSubmitTx $ show err
            SubmitSuccess -> Right ResSubmitTx
