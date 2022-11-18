{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.CLI.Env
  where

import Control.Concurrent.STM (STM)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (RunAsPeer)
import Network.Protocol.Job.Client (JobClient)
import Network.Protocol.Query.Client (QueryClient)

-- | The environment for the Marlowe Runtime CLI.
data Env m = Env
  { envRunHistoryJobClient :: !(RunAsPeer m (JobClient HistoryCommand))
  , envRunHistoryQueryClient :: !(RunAsPeer m (QueryClient HistoryQuery))
  , envRunHistorySyncClient :: !(RunAsPeer m MarloweSyncClient)
  , envRunDiscoveryQueryClient :: !(RunAsPeer m (QueryClient DiscoveryQuery))
  , envRunTxJobClient :: !(RunAsPeer m (JobClient MarloweTxCommand))
  , sigInt :: STM ()
  }
