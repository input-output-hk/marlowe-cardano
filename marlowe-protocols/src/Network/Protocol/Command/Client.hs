{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Command.Client where

import Network.Protocol.Command.Types
import Network.TypedProtocol

newtype CommandClient cmd m a = CommandClient { runCommandClient :: m (ClientStInit cmd m a) }

data ClientStInit cmd m a where
  SendMsgExec
    :: cmd status err result
    -> ClientStCmd cmd status err result m a
    -> ClientStInit cmd m a
  SendMsgResume
    :: CommandId cmd status err result
    -> ClientStCmd cmd status err result m a
    -> ClientStInit cmd m a

deriving instance Functor m => Functor (ClientStInit cmd m)

data ClientStCmd cmd status err result m a = ClientStCmd
  { recvMsgFail    :: err -> m a
  , recvMsgSucceed :: result -> m a
  , recvMsgAwait   :: status -> CommandId cmd status err result -> m (ClientStAwait cmd status err result m a)
  }

deriving instance Functor m => Functor (ClientStCmd cmd status err result m)

data ClientStAwait cmd status err result m a where
  SendMsgPoll :: ClientStCmd cmd status err result m a -> ClientStAwait cmd status err result m a
  SendMsgDone :: a -> ClientStAwait cmd status err result m a

deriving instance Functor m => Functor (ClientStAwait cmd status err result m)

hoistCommandClient
  :: forall cmd m n a
   . Functor m
  => (forall x. m x -> n x)
  -> CommandClient cmd m a
  -> CommandClient cmd n a
hoistCommandClient phi = CommandClient . phi . fmap hoistInit . runCommandClient
  where
  hoistInit = \case
    SendMsgExec cmd stCmd     -> SendMsgExec cmd $ hoistCmd stCmd
    SendMsgResume cmdId stCmd -> SendMsgResume cmdId $ hoistCmd stCmd

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
    SendMsgDone a     -> SendMsgDone a

commandClientPeer
  :: forall cmd m a
   . (Monad m, IsCommand cmd)
  => CommandClient cmd m a
  -> Peer (Command cmd) 'AsClient 'StInit m a
commandClientPeer CommandClient{..} =
  Effect $ peerInit <$> runCommandClient
  where
  peerInit :: ClientStInit cmd m a -> Peer (Command cmd) 'AsClient 'StInit m a
  peerInit = \case
    SendMsgExec cmd stCmd     -> Yield (ClientAgency TokInit) (MsgExec cmd) $ peerCmd (tokFromCmd cmd) stCmd
    SendMsgResume cmdId stCmd -> Yield (ClientAgency TokInit) (MsgResume cmdId) $ peerCmd (tokFromId cmdId) stCmd

  peerCmd
    :: TokCommand cmd status err result
    -> ClientStCmd cmd status err result m a
    -> Peer (Command cmd) 'AsClient ('StCmd status err result) m a
  peerCmd tokCmd ClientStCmd{..} =
    Await (ServerAgency (TokCmd tokCmd)) $ Effect . \case
      MsgFail err           -> Done TokDone <$> recvMsgFail err
      MsgSucceed result     -> Done TokDone <$> recvMsgSucceed result
      MsgAwait status cmdId -> peerAwait tokCmd <$> recvMsgAwait status cmdId

  peerAwait
    :: TokCommand cmd status err result
    -> ClientStAwait cmd status err result m a
    -> Peer (Command cmd) 'AsClient ('StAwait status err result) m a
  peerAwait tokCmd = \case
    SendMsgPoll stCmd -> Yield (ClientAgency (TokAwait tokCmd)) MsgPoll $ peerCmd tokCmd stCmd
    SendMsgDone a     -> Yield (ClientAgency (TokAwait tokCmd)) MsgDone $ Done TokDone a
