{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.JobServer where

import Cardano.Api (CardanoEra(..), CardanoMode, ScriptDataSupportedInEra(..), Tx, TxValidationErrorInMode)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncCommand(..))
import Network.Protocol.Connection (ServerSource(..))
import Network.Protocol.Job.Server
import Ouroboros.Network.Protocol.LocalTxSubmission.Client (SubmitResult(..))
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
   . MonadUnliftIO m
  => ChainSyncJobServerDependencies m
  -> ServerSource (JobServer ChainSyncCommand) m ()
chainSyncJobServer ChainSyncJobServerDependencies{..} = ServerSource $ pure server
  where
    server :: JobServer ChainSyncCommand m ()
    server = liftCommandHandler $ flip either (\case) \case
      SubmitTx era tx -> ((),) <$> do
        result <- submitTxToNodeLocal
          case era of
            ScriptDataInAlonzoEra -> AlonzoEra
            ScriptDataInBabbageEra -> BabbageEra
          tx
        pure case result of
          SubmitFail err -> Left $ show err
          SubmitSuccess -> Right ()
