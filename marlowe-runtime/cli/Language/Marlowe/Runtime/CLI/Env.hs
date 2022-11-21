{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.CLI.Env
  where

import Control.Concurrent.STM (STM)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (RunClient)
import Network.Protocol.Job.Client (JobClient)
import Network.Protocol.Query.Client (QueryClient)

-- | The environment for the Marlowe Runtime CLI.
data Env m = Env
  { envRunHistoryJobClient :: !(RunClient m (JobClient HistoryCommand))
  , envRunHistoryQueryClient :: !(RunClient m (QueryClient HistoryQuery))
  , envRunHistorySyncClient :: !(RunClient m MarloweSyncClient)
  , envRunDiscoveryQueryClient :: !(RunClient m (QueryClient DiscoveryQuery))
  , envRunTxJobClient :: !(RunClient m (JobClient MarloweTxCommand))
  , sigInt :: STM ()
  }
