{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the job protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Job.Client where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Void (Void, absurd)
import Network.Protocol.Job.Types
import Network.Protocol.Peer.Monad (ClientT)
import qualified Network.Protocol.Peer.Monad as PeerT

type JobClientT k = ClientT (Job k)

-- | Create a client that runs a command that cannot await to completion and
-- returns the result.
liftCommand
  :: (Monad m, SingTag t)
  => (Status (t :: k) -> Void)
  -> Command t
  -> JobClientT k 'StInit 'StDone m (Either (JobError t) (JobResult t))
liftCommand elimStatus cmd = PeerT.do
  PeerT.yield $ MsgExec cmd
  PeerT.await \case
    MsgAwait status _ -> absurd $ elimStatus status
    MsgFail err -> pure $ Left err
    MsgSucceed result -> pure $ Right result

-- | Create a client that runs a command to completion and returns the result,
-- waiting as needed.
liftCommandWait
  :: forall m k (t :: k)
   . (MonadIO m, SingTag t)
  => Command (t :: k)
  -> JobClientT k 'StInit 'StDone m (Either (JobError t) (JobResult t))
liftCommandWait cmd = PeerT.do
  PeerT.yield $ MsgExec cmd
  let go :: JobClientT k ('StCmd t) 'StDone m (Either (JobError t) (JobResult t))
      go = PeerT.await \case
        MsgAwait _ _ -> PeerT.do
          liftIO $ threadDelay 100_000
          PeerT.yield MsgPoll
          go
        MsgFail err -> pure $ Left err
        MsgSucceed result -> pure $ Right result
  go
