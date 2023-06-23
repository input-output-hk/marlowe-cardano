{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the job protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Job.Client where

import Control.Concurrent (threadDelay)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor ((<&>))
import Data.Void (Void, absurd)
import Network.Protocol.Job.Server hiding (hoistAttach, hoistAwait, hoistCmd, hoistInit)
import Network.Protocol.Job.Types
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

-- | A generic client for the job protocol.
newtype JobClient cmd m a = JobClient {runJobClient :: m (ClientStInit cmd m a)}
  deriving (Functor)

-- | In the 'StInit' state, the client has agency. It can send:
--
-- * An exec message
-- * An attach message
data ClientStInit cmd m a where
  -- | Tell the server to execute a command in a new job.
  SendMsgExec
    :: cmd status err result
    -> ClientStCmd cmd status err result m a
    -> ClientStInit cmd m a
  -- | Ask to attach to an existing job.
  SendMsgAttach
    :: JobId cmd status err result
    -> ClientStAttach cmd status err result m a
    -> ClientStInit cmd m a

deriving instance (Functor m) => Functor (ClientStInit cmd m)

-- | In the 'StCmd' state, the server has agency. The client must be prepared
-- to handle either:
--
-- * A failure message with an error
-- * A success message with a result
-- * An await message with the current status and job ID
data ClientStCmd cmd status err result m a = ClientStCmd
  { recvMsgFail :: err -> m a
  , recvMsgSucceed :: result -> m a
  , recvMsgAwait :: status -> JobId cmd status err result -> m (ClientStAwait cmd status err result m a)
  }

deriving instance (Functor m) => Functor (ClientStCmd cmd status err result m)

-- | In the 'StAttach' state, the server has agency. The client must be prepared
-- to handle either:
--
-- * An attach failed message.
-- * An attached message.
data ClientStAttach cmd status err result m a = ClientStAttach
  { recvMsgAttachFailed :: m a
  , recvMsgAttached :: m (ClientStCmd cmd status err result m a)
  }

deriving instance (Functor m) => Functor (ClientStAttach cmd status err result m)

-- | In the 'StAwait' state, the client has agency. It can send either:
--
-- * A poll message
-- * A detach message
data ClientStAwait cmd status err result m a where
  -- | Poll the current status of the job and handle the response.
  SendMsgPoll :: ClientStCmd cmd status err result m a -> ClientStAwait cmd status err result m a
  -- | Detach from the current job and return a result.
  SendMsgDetach :: a -> ClientStAwait cmd status err result m a

deriving instance (Functor m) => Functor (ClientStAwait cmd status err result m)

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistJobClient
  :: forall cmd m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> JobClient cmd m a
  -> JobClient cmd n a
hoistJobClient phi = JobClient . phi . fmap hoistInit . runJobClient
  where
    hoistInit = \case
      SendMsgExec cmd stCmd -> SendMsgExec cmd $ hoistCmd stCmd
      SendMsgAttach jobId stAttach -> SendMsgAttach jobId $ hoistAttach stAttach

    hoistAttach
      :: ClientStAttach cmd status err result m a
      -> ClientStAttach cmd status err result n a
    hoistAttach ClientStAttach{..} =
      ClientStAttach
        { recvMsgAttachFailed = phi recvMsgAttachFailed
        , recvMsgAttached = phi $ hoistCmd <$> recvMsgAttached
        }

    hoistCmd
      :: ClientStCmd cmd status err result m a
      -> ClientStCmd cmd status err result n a
    hoistCmd ClientStCmd{..} =
      ClientStCmd
        { recvMsgFail = phi . recvMsgFail
        , recvMsgSucceed = phi . recvMsgSucceed
        , recvMsgAwait = \status -> phi . fmap hoistAwait . recvMsgAwait status
        }

    hoistAwait
      :: ClientStAwait cmd status err result m a
      -> ClientStAwait cmd status err result n a
    hoistAwait = \case
      SendMsgPoll stCmd -> SendMsgPoll $ hoistCmd stCmd
      SendMsgDetach a -> SendMsgDetach a

-- | Interpret a client as a typed-protocols peer.
jobClientPeer
  :: forall cmd m a
   . (Monad m, Command cmd)
  => JobClient cmd m a
  -> PeerTraced (Job cmd) 'AsClient 'StInit m a
jobClientPeer JobClient{..} =
  EffectTraced $ peerInit <$> runJobClient
  where
    peerInit :: ClientStInit cmd m a -> PeerTraced (Job cmd) 'AsClient 'StInit m a
    peerInit = \case
      SendMsgExec cmd stCmd ->
        YieldTraced (ClientAgency TokInit) (MsgExec cmd) $
          Call (ServerAgency $ TokCmd $ tagFromCommand cmd) $
            peerCmd (tagFromCommand cmd) stCmd
      SendMsgAttach jobId stAttach ->
        YieldTraced (ClientAgency TokInit) (MsgAttach jobId) $
          Call (ServerAgency $ TokAttach $ tagFromJobId jobId) $
            peerAttach (tagFromJobId jobId) stAttach

    peerCmd
      :: Tag cmd status err result
      -> ClientStCmd cmd status err result m a
      -> Message (Job cmd) ('StCmd status err result) st
      -> PeerTraced (Job cmd) 'AsClient st m a
    peerCmd tag ClientStCmd{..} =
      EffectTraced . \case
        MsgFail err -> DoneTraced TokDone <$> recvMsgFail err
        MsgSucceed result -> DoneTraced TokDone <$> recvMsgSucceed result
        MsgAwait status jobId -> peerAwait tag <$> recvMsgAwait status jobId

    peerAttach
      :: Tag cmd status err result
      -> ClientStAttach cmd status err result m a
      -> Message (Job cmd) ('StAttach status err result) st
      -> PeerTraced (Job cmd) 'AsClient st m a
    peerAttach tag ClientStAttach{..} =
      EffectTraced . \case
        MsgAttachFailed -> DoneTraced TokDone <$> recvMsgAttachFailed
        MsgAttached ->
          recvMsgAttached <&> \ClientStCmd{..} ->
            AwaitTraced (ServerAgency $ TokCmd tag) \case
              MsgFail err -> Closed TokDone $ recvMsgFail err
              MsgSucceed result -> Closed TokDone $ recvMsgSucceed result
              MsgAwait status jobId -> Receive $ EffectTraced $ peerAwait tag <$> recvMsgAwait status jobId

    peerAwait
      :: Tag cmd status err result
      -> ClientStAwait cmd status err result m a
      -> PeerTraced (Job cmd) 'AsClient ('StAwait status err result) m a
    peerAwait tag = \case
      SendMsgPoll stCmd ->
        YieldTraced (ClientAgency (TokAwait tag)) MsgPoll $
          Call (ServerAgency $ TokCmd tag) $
            peerCmd tag stCmd
      SendMsgDetach a ->
        YieldTraced (ClientAgency (TokAwait tag)) MsgDetach $
          Close TokDone a

-- | Create a client that runs a command that cannot await to completion and
-- returns the result.
liftCommand :: (Monad m) => cmd Void err result -> JobClient cmd m (Either err result)
liftCommand = JobClient . pure . ($ stCmd) . SendMsgExec
  where
    stCmd =
      ClientStCmd
        { recvMsgAwait = absurd
        , recvMsgFail = pure . Left
        , recvMsgSucceed = pure . Right
        }

-- | Create a client that runs a command to completion and returns the result,
-- waiting as needed.
liftCommandWait :: (MonadIO m) => cmd status err result -> JobClient cmd m (Either err result)
liftCommandWait = JobClient . pure . ($ stCmd) . SendMsgExec
  where
    stCmd =
      ClientStCmd
        { recvMsgAwait = \_ _ -> do
            liftIO $ threadDelay 100_000
            pure $ SendMsgPoll stCmd
        , recvMsgFail = pure . Left
        , recvMsgSucceed = pure . Right
        }

serveJobClient
  :: forall cmd m a b
   . (Monad m)
  => JobServer cmd m a
  -> JobClient cmd m b
  -> m (a, b)
serveJobClient JobServer{..} JobClient{..} =
  join $ serveInit <$> runJobServer <*> runJobClient
  where
    serveInit
      :: ServerStInit cmd m a
      -> ClientStInit cmd m b
      -> m (a, b)
    serveInit ServerStInit{..} = \case
      SendMsgExec cmd next -> serveCmd next =<< recvMsgExec cmd
      SendMsgAttach jobId next -> serveAttach next =<< recvMsgAttach jobId

    serveCmd
      :: ClientStCmd cmd status err result m b
      -> ServerStCmd cmd status err result m a
      -> m (a, b)
    serveCmd ClientStCmd{..} = \case
      SendMsgFail err a -> (a,) <$> recvMsgFail err
      SendMsgSucceed result a -> (a,) <$> recvMsgSucceed result
      SendMsgAwait status jobId await -> serveAwait await =<< recvMsgAwait status jobId

    serveAttach
      :: ClientStAttach cmd status err result m b
      -> ServerStAttach cmd status err result m a
      -> m (a, b)
    serveAttach ClientStAttach{..} = \case
      SendMsgAttachFailed a -> (a,) <$> recvMsgAttachFailed
      SendMsgAttached cmd -> flip serveCmd cmd =<< recvMsgAttached

    serveAwait
      :: ServerStAwait cmd status err result m a
      -> ClientStAwait cmd status err result m b
      -> m (a, b)
    serveAwait ServerStAwait{..} = \case
      SendMsgPoll cmd -> serveCmd cmd =<< recvMsgPoll
      SendMsgDetach b -> (,b) <$> recvMsgDetach
