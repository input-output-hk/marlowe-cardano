{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.CLI.Env
  where

import Control.Concurrent.STM (STM)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (RunClient)
import Network.Protocol.Job.Client (JobClient)

-- | The environment for the Marlowe Runtime CLI.
data Env m = Env
  { envRunHistorySyncClient :: !(RunClient m MarloweSyncClient)
  , envRunTxJobClient :: !(RunClient m (JobClient MarloweTxCommand))
  , sigInt :: STM ()
  }
