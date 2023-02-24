{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Sync.Server
  where

import Language.Marlowe.Protocol.Sync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion)
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)
import Network.TypedProtocol

newtype MarloweSyncServer m a = MarloweSyncServer { runMarloweSyncServer :: m (ServerStInit m a) }
  deriving Functor

data ServerStInit m a = ServerStInit
  { recvMsgFollowContract :: ContractId -> m (ServerStFollow m a)
  , recvMsgIntersect      :: forall v. ContractId -> MarloweVersion v -> [BlockHeader] -> m (ServerStIntersect v m a)
  } deriving Functor

data ServerStFollow m a where
  SendMsgContractFound :: BlockHeader -> MarloweVersion v -> CreateStep v -> ServerStIdle v m a -> ServerStFollow m a
  SendMsgContractNotFound :: a -> ServerStFollow m a

deriving instance Functor m => Functor (ServerStFollow m)

data ServerStIntersect v m a where
  SendMsgIntersectFound :: BlockHeader -> ServerStIdle v m a -> ServerStIntersect v m a
  SendMsgIntersectNotFound :: a -> ServerStIntersect v m a

deriving instance Functor m => Functor (ServerStIntersect v m)

data ServerStIdle v m a = ServerStIdle
  { recvMsgRequestNext :: m (ServerStNext v m a)
  , recvMsgDone        :: m a
  } deriving Functor

data ServerStNext v m a where
  SendMsgRollForward :: BlockHeader -> [ContractStep v] -> ServerStIdle v m a -> ServerStNext v m a
  SendMsgRollBackward :: BlockHeader -> ServerStIdle v m a -> ServerStNext v m a
  SendMsgRollBackCreation :: a -> ServerStNext v m a
  SendMsgWait :: ServerStWait v m a -> ServerStNext v m a

deriving instance Functor m => Functor (ServerStNext v m)

data ServerStWait v m a = ServerStWait
  { recvMsgPoll   :: m (ServerStNext v m a)
  , recvMsgCancel :: m (ServerStIdle v m a)
  } deriving Functor

hoistMarloweSyncServer
  :: forall m n a
   . Functor m
  => (forall x. m x -> n x)
  -> MarloweSyncServer m a
  -> MarloweSyncServer n a
hoistMarloweSyncServer nat = MarloweSyncServer . nat . fmap hoistInit . runMarloweSyncServer
  where
    hoistInit :: ServerStInit m a -> ServerStInit n a
    hoistInit ServerStInit{..} = ServerStInit
      { recvMsgFollowContract = nat . fmap hoistFollow . recvMsgFollowContract
      , recvMsgIntersect = \contractId version -> nat . fmap hoistIntersect . recvMsgIntersect contractId version
      }

    hoistIntersect :: ServerStIntersect v m a -> ServerStIntersect v n a
    hoistIntersect = \case
      SendMsgIntersectFound blockHeader idle -> SendMsgIntersectFound blockHeader $ hoistIdle idle
      SendMsgIntersectNotFound a             -> SendMsgIntersectNotFound a

    hoistFollow :: ServerStFollow m a -> ServerStFollow n a
    hoistFollow = \case
      SendMsgContractFound blockHeader version createStep idle -> SendMsgContractFound blockHeader version createStep $ hoistIdle idle
      SendMsgContractNotFound a -> SendMsgContractNotFound a

    hoistIdle :: ServerStIdle v m a -> ServerStIdle v n a
    hoistIdle ServerStIdle{..} = ServerStIdle
      { recvMsgRequestNext = nat $ fmap hoistNext recvMsgRequestNext
      , recvMsgDone = nat recvMsgDone
      }

    hoistNext :: ServerStNext v m a -> ServerStNext v n a
    hoistNext = \case
      SendMsgRollForward blockHeader steps idle -> SendMsgRollForward blockHeader steps $ hoistIdle idle
      SendMsgRollBackward blockHeader idle      -> SendMsgRollBackward blockHeader $ hoistIdle idle
      SendMsgRollBackCreation a                 -> SendMsgRollBackCreation a
      SendMsgWait wait                          -> SendMsgWait $ hoistWait wait

    hoistWait :: ServerStWait v m a -> ServerStWait v n a
    hoistWait ServerStWait{..} = ServerStWait
      { recvMsgPoll = nat $ fmap hoistNext recvMsgPoll
      , recvMsgCancel = nat $ fmap hoistIdle recvMsgCancel
      }

marloweSyncServerPeer :: forall m a. Functor m => MarloweSyncServer m a -> Peer MarloweSync 'AsServer 'StInit m a
marloweSyncServerPeer = Effect . fmap peerInit . runMarloweSyncServer
  where
    peerInit :: ServerStInit m a -> Peer MarloweSync 'AsServer 'StInit m a
    peerInit ServerStInit{..} = Await (ClientAgency TokInit) $ Effect . \case
      MsgFollowContract contractId ->
        peerFollow <$> recvMsgFollowContract contractId
      MsgIntersect contractId version headers ->
        peerIntersect version <$> recvMsgIntersect contractId version headers

    peerFollow :: ServerStFollow m a -> Peer MarloweSync 'AsServer 'StFollow m a
    peerFollow = \case
      SendMsgContractFound blockHeader version createStep idle ->
        Yield (ServerAgency TokFollow) (MsgContractFound blockHeader version createStep) $
        peerIdle version idle
      SendMsgContractNotFound a ->
        Yield (ServerAgency TokFollow) MsgContractNotFound $ Done TokDone a

    peerIntersect :: MarloweVersion v -> ServerStIntersect v m a -> Peer MarloweSync 'AsServer ('StIntersect v) m a
    peerIntersect version = \case
      SendMsgIntersectFound blockHeader idle ->
        Yield (ServerAgency (TokIntersect version)) (MsgIntersectFound blockHeader) $
        peerIdle version idle
      SendMsgIntersectNotFound a ->
        Yield (ServerAgency (TokIntersect version)) MsgIntersectNotFound $ Done TokDone a

    peerIdle :: MarloweVersion v -> ServerStIdle v m a -> Peer MarloweSync 'AsServer ('StIdle v) m a
    peerIdle version ServerStIdle{..} = Await (ClientAgency (TokIdle version)) $ Effect . \case
      MsgDone        -> Done TokDone <$> recvMsgDone
      MsgRequestNext -> peerNext version <$> recvMsgRequestNext

    peerNext :: MarloweVersion v -> ServerStNext v m a -> Peer MarloweSync 'AsServer ('StNext v) m a
    peerNext version = \case
      SendMsgRollForward blockHeader steps idle ->
        Yield (ServerAgency (TokNext version)) (MsgRollForward blockHeader steps) $
        peerIdle version idle
      SendMsgRollBackward blockHeader idle ->
        Yield (ServerAgency (TokNext version)) (MsgRollBackward blockHeader) $
        peerIdle version idle
      SendMsgRollBackCreation a ->
        Yield (ServerAgency (TokNext version)) MsgRollBackCreation $
        Done TokDone a
      SendMsgWait wait ->
        Yield (ServerAgency (TokNext version)) MsgWait $
        peerWait version wait

    peerWait :: MarloweVersion v -> ServerStWait v m a -> Peer MarloweSync 'AsServer ('StWait v) m a
    peerWait version ServerStWait{..} = Await (ClientAgency (TokWait version)) $ Effect . \case
      MsgPoll   -> peerNext version <$> recvMsgPoll
      MsgCancel -> peerIdle version <$> recvMsgCancel
