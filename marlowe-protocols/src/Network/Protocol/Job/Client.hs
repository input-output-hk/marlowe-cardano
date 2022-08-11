{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | A generc client for the job protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Job.Client where

import Data.Void (Void, absurd)
import Network.Protocol.Job.Types
import Network.TypedProtocol

-- | A generic client for the job protocol.
newtype JobClient cmd m a = JobClient { runJobClient :: m (ClientStInit cmd m a) }

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
    -> ClientStCmd cmd status err result m a
    -> ClientStInit cmd m a

deriving instance Functor m => Functor (ClientStInit cmd m)

-- | In the 'StCmd' state, the server has agency. The client must be prepared
-- to handle either:
--
-- * A failure message with an error
-- * A success message with a result
-- * An await message with the current status and job ID
data ClientStCmd cmd status err result m a = ClientStCmd
  { recvMsgFail    :: err -> m a
  , recvMsgSucceed :: result -> m a
  , recvMsgAwait   :: status -> JobId cmd status err result -> m (ClientStAwait cmd status err result m a)
  }

deriving instance Functor m => Functor (ClientStCmd cmd status err result m)

-- | In the 'StAwait' state, the client has agency. It can send either:
--
-- * A poll message
-- * A detach message
data ClientStAwait cmd status err result m a where
  -- | Poll the current status of the job and handle the response.
  SendMsgPoll :: ClientStCmd cmd status err result m a -> ClientStAwait cmd status err result m a

  -- | Detach from the current job and return a result.
  SendMsgDetach :: a -> ClientStAwait cmd status err result m a

deriving instance Functor m => Functor (ClientStAwait cmd status err result m)

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistJobClient
  :: forall cmd m n a
   . Functor m
  => (forall x. m x -> n x)
  -> JobClient cmd m a
  -> JobClient cmd n a
hoistJobClient phi = JobClient . phi . fmap hoistInit . runJobClient
  where
  hoistInit = \case
    SendMsgExec cmd stCmd     -> SendMsgExec cmd $ hoistCmd stCmd
    SendMsgAttach jobId stCmd -> SendMsgAttach jobId $ hoistCmd stCmd

  hoistCmd
    :: ClientStCmd cmd status err result m a
    -> ClientStCmd cmd status err result n a
  hoistCmd ClientStCmd{..} = ClientStCmd
    { recvMsgFail = phi . recvMsgFail
    , recvMsgSucceed = phi . recvMsgSucceed
    , recvMsgAwait = \status -> phi . fmap hoistAwait . recvMsgAwait status
    }

  hoistAwait
    :: ClientStAwait cmd status err result m a
    -> ClientStAwait cmd status err result n a
  hoistAwait = \case
    SendMsgPoll stCmd -> SendMsgPoll $ hoistCmd stCmd
    SendMsgDetach a   -> SendMsgDetach a

-- | Interpret a client as a typed-protocols peer.
commandClientPeer
  :: forall cmd m a
   . (Monad m, Command cmd)
  => JobClient cmd m a
  -> Peer (Job cmd) 'AsClient 'StInit m a
commandClientPeer JobClient{..} =
  Effect $ peerInit <$> runJobClient
  where
  peerInit :: ClientStInit cmd m a -> Peer (Job cmd) 'AsClient 'StInit m a
  peerInit = \case
    SendMsgExec cmd stCmd     -> Yield (ClientAgency TokInit) (MsgExec cmd) $ peerCmd (tokFromCmd cmd) stCmd
    SendMsgAttach jobId stCmd -> Yield (ClientAgency TokInit) (MsgAttach jobId) $ peerCmd (tokFromId jobId) stCmd

  peerCmd
    :: TokCommand cmd status err result
    -> ClientStCmd cmd status err result m a
    -> Peer (Job cmd) 'AsClient ('StCmd status err result) m a
  peerCmd tokCmd ClientStCmd{..} =
    Await (ServerAgency (TokCmd tokCmd)) $ Effect . \case
      MsgFail _ err         -> Done TokDone <$> recvMsgFail err
      MsgSucceed _ result   -> Done TokDone <$> recvMsgSucceed result
      MsgAwait status jobId -> peerAwait tokCmd <$> recvMsgAwait status jobId

  peerAwait
    :: TokCommand cmd status err result
    -> ClientStAwait cmd status err result m a
    -> Peer (Job cmd) 'AsClient ('StAwait status err result) m a
  peerAwait tokCmd = \case
    SendMsgPoll stCmd -> Yield (ClientAgency (TokAwait tokCmd)) MsgPoll $ peerCmd tokCmd stCmd
    SendMsgDetach a   -> Yield (ClientAgency (TokAwait tokCmd)) MsgDetach $ Done TokDone a

-- | Create a client that runs a command that cannot await to completion and
-- returns the result.
liftCommand :: Monad m => cmd Void err result -> JobClient cmd m (Either err result)
liftCommand = JobClient . pure . ($ stCmd) . SendMsgExec
  where
    stCmd = ClientStCmd
      { recvMsgAwait = absurd
      , recvMsgFail = pure . Left
      , recvMsgSucceed = pure . Right
      }
