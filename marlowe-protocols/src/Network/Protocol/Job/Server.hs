{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the job protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Job.Server where

import Control.Monad.Trans (MonadTrans (..))
import Network.Protocol.Job.Types
import Network.Protocol.Peer.Monad (ServerT)
import qualified Network.Protocol.Peer.Monad as PeerT

type JobServerT k = ServerT (Job k)

-- | Lift a function that executes a command directly into a command server.
liftCommandHandler
  :: (Monad m, TagKind k)
  => (forall (t :: k). Either (Command t) (JobId t) -> m (a, Either (JobError t) (JobResult t)))
  -> JobServerT k 'StInit 'StDone m a
liftCommandHandler handle = PeerT.await \case
  MsgExec cmd -> withSingTag (commandTag cmd) PeerT.do
    (a, e) <- lift $ handle $ Left cmd
    PeerT.yield case e of
      Left err -> MsgFail err
      Right result -> MsgSucceed result
    pure a
  MsgAttach jobId -> withSingTag (jobIdTag jobId) PeerT.do
    (a, e) <- lift $ handle $ Right jobId
    PeerT.yield MsgAttached
    PeerT.yield case e of
      Left err -> MsgFail err
      Right result -> MsgSucceed result
    pure a
