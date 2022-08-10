{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Command.Server where

import Network.Protocol.Command.Types
import Network.TypedProtocol

newtype CommandServer cmd m a = CommandServer { runCommandServer :: m (ServerStInit cmd m a) }

data ServerStInit cmd m a = ServerStInit
  { recvMsgExec
    :: forall status err result
     . cmd status err result
    -> m (ServerStCmd cmd status err result m a)
  , recvMsgResume
    :: forall status err result
     . CommandId cmd status err result
    -> m (ServerStCmd cmd status err result m a)
  }

deriving instance Functor m => Functor (ServerStInit cmd m)

data ServerStCmd cmd status err result m a where
  SendMsgFail    :: err -> a -> ServerStCmd cmd status err result m a
  SendMsgSucceed :: result -> a -> ServerStCmd cmd status err result m a
  SendMsgAwait
    :: status
    -> CommandId cmd status err result
    -> ServerStAwait cmd status err result m a
    -> ServerStCmd cmd status err result m a

deriving instance Functor m => Functor (ServerStCmd cmd status err result m)

data ServerStAwait cmd status err result m a = ServerStAwait
  { recvMsgPoll :: m (ServerStCmd cmd status err result m a)
  , recvMsgDone :: m a
  }

deriving instance Functor m => Functor (ServerStAwait cmd status err result m)

hoistCommandServer
  :: forall cmd m n a
   . Functor m
  => (forall x. m x -> n x)
  -> CommandServer cmd m a
  -> CommandServer cmd n a
hoistCommandServer phi = CommandServer . phi . fmap hoistInit . runCommandServer
  where
  hoistInit ServerStInit{..} = ServerStInit
    { recvMsgExec = phi . fmap hoistCmd . recvMsgExec
    , recvMsgResume = phi . fmap hoistCmd . recvMsgResume
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
    , recvMsgDone = phi recvMsgDone
    }

commandServerPeer
  :: forall cmd m a
   . (Monad m, IsCommand cmd)
  => CommandServer cmd m a
  -> Peer (Command cmd) 'AsServer 'StInit m a
commandServerPeer CommandServer{..} =
  Effect $ peerInit <$> runCommandServer
  where
  peerInit :: ServerStInit cmd m a -> Peer (Command cmd) 'AsServer 'StInit m a
  peerInit ServerStInit{..} =
    Await (ClientAgency TokInit) $ Effect . \case
      MsgExec cmd     -> peerCmd (tokFromCmd cmd) <$> recvMsgExec cmd
      MsgResume cmdId -> peerCmd (tokFromId cmdId) <$> recvMsgResume cmdId

  peerCmd
    :: TokCommand cmd status err result
    -> ServerStCmd cmd status err result m a
    -> Peer (Command cmd) 'AsServer ('StCmd status err result) m a
  peerCmd tokCmd = \case
    SendMsgFail err a -> Yield (ServerAgency (TokCmd tokCmd)) (MsgFail err) $ Done TokDone a
    SendMsgSucceed result a -> Yield (ServerAgency (TokCmd tokCmd)) (MsgSucceed result) $ Done TokDone a
    SendMsgAwait status cmdId await -> Yield (ServerAgency (TokCmd tokCmd)) (MsgAwait status cmdId) $ peerAwait tokCmd await

  peerAwait
    :: TokCommand cmd status err result
    -> ServerStAwait cmd status err result m a
    -> Peer (Command cmd) 'AsServer ('StAwait status err result) m a
  peerAwait tokCmd ServerStAwait{..} =
    Await (ClientAgency (TokAwait tokCmd)) $ Effect . \case
      MsgPoll -> peerCmd tokCmd <$> recvMsgPoll
      MsgDone -> Done TokDone <$> recvMsgDone
