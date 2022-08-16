{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | A generc server for the job protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Job.Server where

import Network.Protocol.Job.Types
import Network.TypedProtocol

-- | A generic server for the job protocol.
newtype JobServer cmd m a = JobServer { runJobServer :: m (ServerStInit cmd m a) }

-- | In the 'StInit' state, the client has agency. The server must be prepared
-- to handle either:
--
-- * An exec message
-- * An attach message
data ServerStInit cmd m a = ServerStInit
  { recvMsgExec
    :: forall status err result
     . cmd status err result
    -> m (ServerStCmd cmd status err result m a)
  , recvMsgAttach
    :: forall status err result
     . JobId cmd status err result
    -> m (ServerStCmd cmd status err result m a)
  }

deriving instance Functor m => Functor (ServerStInit cmd m)

-- | In the 'StCmd' state, the server has agency. It is obtaining the status of
-- the job associated with a previously issued command, and may send:
--
-- * A failure message with an error
-- * A success message with a result
-- * An await message with the current status and job ID
data ServerStCmd cmd status err result m a where

  -- | Send a failure message to the client and terminate the protocol.
  SendMsgFail    :: err -> a -> ServerStCmd cmd status err result m a

  -- | Send a success message to the client and terminate the protocol.
  SendMsgSucceed :: result -> a -> ServerStCmd cmd status err result m a

  -- | Send an await message to the client and await a follow-up message from
  -- the client.
  SendMsgAwait
    :: status
    -> JobId cmd status err result
    -> ServerStAwait cmd status err result m a
    -> ServerStCmd cmd status err result m a

deriving instance Functor m => Functor (ServerStCmd cmd status err result m)

-- | In the 'StAwait' state, the client has agency. The server must be prepared
-- to handle either:
--
-- * A poll message
-- * A detach message
data ServerStAwait cmd status err result m a = ServerStAwait
  { recvMsgPoll   :: m (ServerStCmd cmd status err result m a)
  , recvMsgDetach :: m a
  }

deriving instance Functor m => Functor (ServerStAwait cmd status err result m)

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistJobServer
  :: forall cmd m n a
   . Functor m
  => (forall x. m x -> n x)
  -> JobServer cmd m a
  -> JobServer cmd n a
hoistJobServer phi = JobServer . phi . fmap hoistInit . runJobServer
  where
  hoistInit ServerStInit{..} = ServerStInit
    { recvMsgExec = phi . fmap hoistCmd . recvMsgExec
    , recvMsgAttach = phi . fmap hoistCmd . recvMsgAttach
    }

  hoistCmd
    :: ServerStCmd cmd status err result m a
    -> ServerStCmd cmd status err result n a
  hoistCmd = \case
    SendMsgFail err a               -> SendMsgFail err a
    SendMsgSucceed result a         -> SendMsgSucceed result a
    SendMsgAwait status cmdId await -> SendMsgAwait status cmdId $ hoistAwait await

  hoistAwait
    :: ServerStAwait cmd status err result m a
    -> ServerStAwait cmd status err result n a
  hoistAwait ServerStAwait{..} = ServerStAwait
    { recvMsgPoll = phi $ hoistCmd <$> recvMsgPoll
    , recvMsgDetach = phi recvMsgDetach
    }

-- | Interpret a server as a typed-protocols peer.
jobServerPeer
  :: forall cmd m a
   . (Monad m, Command cmd)
  => JobServer cmd m a
  -> Peer (Job cmd) 'AsServer 'StInit m a
jobServerPeer JobServer{..} =
  Effect $ peerInit <$> runJobServer
  where
  peerInit :: ServerStInit cmd m a -> Peer (Job cmd) 'AsServer 'StInit m a
  peerInit ServerStInit{..} =
    Await (ClientAgency TokInit) $ Effect . \case
      MsgExec cmd     -> peerCmd (tagFromCommand cmd) <$> recvMsgExec cmd
      MsgAttach cmdId -> peerCmd (tagFromJobId cmdId) <$> recvMsgAttach cmdId

  peerCmd
    :: Tag cmd status err result
    -> ServerStCmd cmd status err result m a
    -> Peer (Job cmd) 'AsServer ('StCmd status err result) m a
  peerCmd tag = \case
    SendMsgFail err a               -> Yield (ServerAgency (TokCmd tag)) (MsgFail err) $ Done TokDone a
    SendMsgSucceed result a         -> Yield (ServerAgency (TokCmd tag)) (MsgSucceed result) $ Done TokDone a
    SendMsgAwait status cmdId await -> Yield (ServerAgency (TokCmd tag)) (MsgAwait status cmdId) $ peerAwait tag await

  peerAwait
    :: Tag cmd status err result
    -> ServerStAwait cmd status err result m a
    -> Peer (Job cmd) 'AsServer ('StAwait status err result) m a
  peerAwait tokCmd ServerStAwait{..} =
    Await (ClientAgency (TokAwait tokCmd)) $ Effect . \case
      MsgPoll   -> peerCmd tokCmd <$> recvMsgPoll
      MsgDetach -> Done TokDone <$> recvMsgDetach

-- | Lift a function that executes a command directly into a command server.
liftCommandHandler
  :: Monad m
  => (forall status err result. Either (cmd status err result) (JobId cmd status err result) -> m (a, Either err result))
  -> JobServer cmd m a
liftCommandHandler handle = JobServer $ pure $ ServerStInit
  { recvMsgExec = \cmd -> do
      (a, e) <- handle $ Left cmd
      pure case e of
        Left err     -> SendMsgFail err a
        Right result -> SendMsgSucceed result a
  , recvMsgAttach = \cmdId -> do
      (a, e) <- handle $ Right cmdId
      pure case e of
        Left err     -> SendMsgFail err a
        Right result -> SendMsgSucceed result a
  }
