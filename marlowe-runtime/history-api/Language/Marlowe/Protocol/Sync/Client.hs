{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Sync.Client
  where

import Language.Marlowe.Protocol.Sync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion)
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)
import Network.TypedProtocol

newtype MarloweSyncClient m a = MarloweSyncClient { runMarloweSyncClient :: m (ClientStInit m a) }
  deriving Functor

data ClientStInit m a where
  SendMsgFollowContract :: ContractId -> ClientStFollow m a -> ClientStInit m a
  SendMsgIntersect :: ContractId -> MarloweVersion v -> [BlockHeader] -> ClientStIntersect v m a -> ClientStInit m a

deriving instance Functor m => Functor (ClientStInit m)

data ClientStFollow m a = ClientStFollow
  { recvMsgContractFound    :: forall v. BlockHeader -> MarloweVersion v -> CreateStep v -> m (ClientStIdle v m a)
  , recvMsgContractNotFound :: m a
  } deriving Functor

data ClientStIntersect v m a = ClientStIntersect
  { recvMsgIntersectFound    :: BlockHeader -> m (ClientStIdle v m a)
  , recvMsgIntersectNotFound :: m a
  } deriving Functor

data ClientStIdle v m a where
  SendMsgRequestNext :: ClientStNext v m a -> ClientStIdle v m a
  SendMsgDone :: a -> ClientStIdle v m a

deriving instance Functor m => Functor (ClientStIdle v m)

data ClientStNext v m a = ClientStNext
  { recvMsgRollForward      :: BlockHeader -> [ContractStep v] -> m (ClientStIdle v m a)
  , recvMsgRollBackward     :: BlockHeader -> m (ClientStIdle v m a)
  , recvMsgRollBackCreation :: m a
  , recvMsgWait             :: m (ClientStWait v m a)
  } deriving Functor

data ClientStWait v m a where
  SendMsgPoll :: ClientStNext v m a -> ClientStWait v m a
  SendMsgCancel :: ClientStIdle v m a -> ClientStWait v m a

deriving instance Functor m => Functor (ClientStWait v m)

hoistMarloweSyncClient
  :: forall m n a
   . Functor m
  => (forall x. m x -> n x)
  -> MarloweSyncClient m a
  -> MarloweSyncClient n a
hoistMarloweSyncClient nat = MarloweSyncClient . nat . fmap hoistInit . runMarloweSyncClient
  where
    hoistInit :: ClientStInit m a -> ClientStInit n a
    hoistInit = \case
      SendMsgFollowContract contractId follow -> SendMsgFollowContract contractId $ hoistFollow follow
      SendMsgIntersect contractId version headers intersect -> SendMsgIntersect contractId version headers $ hoistIntersect intersect

    hoistIntersect :: ClientStIntersect v m a -> ClientStIntersect v n a
    hoistIntersect ClientStIntersect{..} = ClientStIntersect
      { recvMsgIntersectFound = nat . fmap hoistIdle . recvMsgIntersectFound
      , recvMsgIntersectNotFound = nat recvMsgIntersectNotFound
      }

    hoistFollow :: ClientStFollow m a -> ClientStFollow n a
    hoistFollow ClientStFollow{..} = ClientStFollow
      { recvMsgContractFound = \header version -> nat . fmap hoistIdle . recvMsgContractFound header version
      , recvMsgContractNotFound = nat recvMsgContractNotFound
      }

    hoistIdle :: ClientStIdle v m a -> ClientStIdle v n a
    hoistIdle = \case
      SendMsgRequestNext next -> SendMsgRequestNext $ hoistNext next
      SendMsgDone a           -> SendMsgDone a

    hoistNext :: ClientStNext v m a -> ClientStNext v n a
    hoistNext ClientStNext{..} = ClientStNext
      { recvMsgRollForward = \header -> nat . fmap hoistIdle . recvMsgRollForward header
      , recvMsgRollBackward = nat . fmap hoistIdle . recvMsgRollBackward
      , recvMsgRollBackCreation = nat recvMsgRollBackCreation
      , recvMsgWait = nat $ fmap hoistWait recvMsgWait
      }

    hoistWait :: ClientStWait v m a -> ClientStWait v n a
    hoistWait = \case
      SendMsgPoll next   -> SendMsgPoll $ hoistNext next
      SendMsgCancel idle -> SendMsgCancel $ hoistIdle idle

marloweSyncClientPeer :: forall m a. Functor m => MarloweSyncClient m a -> Peer MarloweSync 'AsClient 'StInit m a
marloweSyncClientPeer = Effect . fmap peerInit . runMarloweSyncClient
  where
    peerInit :: ClientStInit m a -> Peer MarloweSync 'AsClient 'StInit m a
    peerInit = \case
      SendMsgFollowContract contractId follow ->
        Yield (ClientAgency TokInit) (MsgFollowContract contractId) $
        peerFollow follow
      SendMsgIntersect contractId version headers intersect ->
        Yield (ClientAgency TokInit) (MsgIntersect contractId version headers) $
        peerIntersect version intersect

    peerFollow :: ClientStFollow m a -> Peer MarloweSync 'AsClient 'StFollow m a
    peerFollow ClientStFollow{..} = Await (ServerAgency TokFollow) $ Effect . \case
      MsgContractFound header version step ->
        peerIdle version <$> recvMsgContractFound header version step
      MsgContractNotFound ->
        Done TokDone <$> recvMsgContractNotFound

    peerIntersect :: MarloweVersion v -> ClientStIntersect v m a -> Peer MarloweSync 'AsClient ('StIntersect v) m a
    peerIntersect version ClientStIntersect{..} =
      Await (ServerAgency (TokIntersect version)) $ Effect . \case
        MsgIntersectFound header -> peerIdle version <$> recvMsgIntersectFound header
        MsgIntersectNotFound     -> Done TokDone <$> recvMsgIntersectNotFound

    peerIdle :: MarloweVersion v -> ClientStIdle v m a -> Peer MarloweSync 'AsClient ('StIdle v) m a
    peerIdle version = \case
      SendMsgRequestNext next ->
        Yield (ClientAgency (TokIdle version)) MsgRequestNext $ peerNext version next
      SendMsgDone a ->
        Yield (ClientAgency (TokIdle version)) MsgDone $ Done TokDone a

    peerNext :: MarloweVersion v -> ClientStNext v m a -> Peer MarloweSync 'AsClient ('StNext v) m a
    peerNext version ClientStNext{..} = Await (ServerAgency (TokNext version)) $ Effect . \case
      MsgRollForward header steps -> peerIdle version <$> recvMsgRollForward header steps
      MsgRollBackward header      -> peerIdle version <$> recvMsgRollBackward header
      MsgRollBackCreation         -> Done TokDone <$> recvMsgRollBackCreation
      MsgWait                     -> peerWait version <$> recvMsgWait

    peerWait :: MarloweVersion v -> ClientStWait v m a -> Peer MarloweSync 'AsClient ('StWait v) m a
    peerWait version = \case
      SendMsgPoll next ->
        Yield (ClientAgency (TokWait version)) MsgPoll $ peerNext version next
      SendMsgCancel idle ->
        Yield (ClientAgency (TokWait version)) MsgCancel $ peerIdle version idle
