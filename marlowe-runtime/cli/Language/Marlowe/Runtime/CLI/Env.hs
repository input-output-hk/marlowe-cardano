{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.CLI.Env
  where

import Control.Concurrent.STM (STM)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (SomeClientConnector)
import Network.Protocol.Job.Client (JobClient)

-- | The environment for the Marlowe Runtime CLI.
data Env m = Env
  { marloweSyncConnector :: SomeClientConnector MarloweSyncClient m
  , txJobConnector :: SomeClientConnector (JobClient MarloweTxCommand) m
  , sigInt :: STM ()
  }
