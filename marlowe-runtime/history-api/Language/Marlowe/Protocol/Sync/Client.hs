{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Sync.Client where

import Control.Monad (join)
import Language.Marlowe.Protocol.Sync.Server
import Language.Marlowe.Protocol.Sync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion)
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)
import Network.Protocol.Peer.Trace
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

marloweSyncClientPeer
  :: forall m a
   . Functor m
  => MarloweSyncClient m a
  -> PeerTraced MarloweSync 'AsClient 'StInit m a
marloweSyncClientPeer = EffectTraced . fmap peerInit . runMarloweSyncClient
  where
    peerInit :: ClientStInit m a -> PeerTraced MarloweSync 'AsClient 'StInit m a
    peerInit = \case
      SendMsgFollowContract contractId follow ->
        YieldTraced (ClientAgency TokInit) (MsgFollowContract contractId)
          $ Call (ServerAgency TokFollow)
          $ peerFollow follow
      SendMsgIntersect contractId version headers intersect ->
        YieldTraced (ClientAgency TokInit) (MsgIntersect contractId version headers)
          $ Call (ServerAgency $ TokIntersect version)
          $ peerIntersect version intersect

    peerFollow
      :: ClientStFollow m a
      -> Message MarloweSync 'StFollow st
      -> PeerTraced MarloweSync 'AsClient st m a
    peerFollow ClientStFollow{..} = EffectTraced . \case
      MsgContractFound header version step ->
        peerIdle version <$> recvMsgContractFound header version step
      MsgContractNotFound ->
        DoneTraced TokDone <$> recvMsgContractNotFound

    peerIntersect
      :: MarloweVersion v
      -> ClientStIntersect v m a
      -> Message MarloweSync ('StIntersect v) st
      -> PeerTraced MarloweSync 'AsClient st m a
    peerIntersect version ClientStIntersect{..} = EffectTraced . \case
      MsgIntersectFound header -> peerIdle version <$> recvMsgIntersectFound header
      MsgIntersectNotFound -> DoneTraced TokDone <$> recvMsgIntersectNotFound

    peerIdle :: MarloweVersion v -> ClientStIdle v m a -> PeerTraced MarloweSync 'AsClient ('StIdle v) m a
    peerIdle version = \case
      SendMsgRequestNext next ->
        YieldTraced (ClientAgency (TokIdle version)) MsgRequestNext
          $ Call (ServerAgency $ TokNext version)
          $ peerNext version next
      SendMsgDone a ->
        YieldTraced (ClientAgency (TokIdle version)) MsgDone
          $ Close TokDone a

    peerNext
      :: MarloweVersion v
      -> ClientStNext v m a
      -> Message MarloweSync ('StNext v) st
      -> PeerTraced MarloweSync 'AsClient st m a
    peerNext version ClientStNext{..} = EffectTraced . \case
      MsgRollForward header steps -> peerIdle version <$> recvMsgRollForward header steps
      MsgRollBackward header -> peerIdle version <$> recvMsgRollBackward header
      MsgRollBackCreation -> DoneTraced TokDone <$> recvMsgRollBackCreation
      MsgWait -> peerWait version <$> recvMsgWait

    peerWait
      :: MarloweVersion v
      -> ClientStWait v m a
      -> PeerTraced MarloweSync 'AsClient ('StWait v) m a
    peerWait version = \case
      SendMsgPoll next ->
        YieldTraced (ClientAgency (TokWait version)) MsgPoll
          $ Call (ServerAgency $ TokNext version)
          $ peerNext version next
      SendMsgCancel idle ->
        YieldTraced (ClientAgency (TokWait version)) MsgCancel
          $ Cast
          $ peerIdle version idle

serveMarloweSyncClient
  :: forall m a b
   . Monad m
  => MarloweSyncServer m a
  -> MarloweSyncClient m b
  -> m (a, b)
serveMarloweSyncClient MarloweSyncServer{..} MarloweSyncClient{..} =
  join $ serveInit <$> runMarloweSyncServer <*> runMarloweSyncClient
  where
    serveInit :: ServerStInit m a -> ClientStInit m b -> m (a, b)
    serveInit ServerStInit{..} = \case
      SendMsgFollowContract contractId follow -> serveFollow follow =<< recvMsgFollowContract contractId
      SendMsgIntersect contractId version blocks intersect -> serveIntersect intersect =<< recvMsgIntersect contractId version blocks

    serveFollow :: ClientStFollow m b -> ServerStFollow m a -> m (a, b)
    serveFollow ClientStFollow{..} = \case
      SendMsgContractFound block version createStep idle -> serveIdle idle =<< recvMsgContractFound block version createStep
      SendMsgContractNotFound a -> (a,) <$> recvMsgContractNotFound

    serveIntersect :: ClientStIntersect v m b -> ServerStIntersect v m a -> m (a, b)
    serveIntersect ClientStIntersect{..} = \case
      SendMsgIntersectFound block idle -> serveIdle idle =<< recvMsgIntersectFound block
      SendMsgIntersectNotFound a -> (a,) <$> recvMsgIntersectNotFound

    serveIdle :: ServerStIdle v m a -> ClientStIdle v m b -> m (a, b)
    serveIdle ServerStIdle{..} = \case
      SendMsgRequestNext next -> serveNext next =<< recvMsgRequestNext
      SendMsgDone b -> (,b) <$> recvMsgDone

    serveNext :: ClientStNext v m b -> ServerStNext v m a -> m (a, b)
    serveNext ClientStNext{..} = \case
      SendMsgRollForward block steps idle -> serveIdle idle =<< recvMsgRollForward block steps
      SendMsgRollBackward point idle -> serveIdle idle =<< recvMsgRollBackward point
      SendMsgRollBackCreation a -> (a,) <$> recvMsgRollBackCreation
      SendMsgWait poll -> serveWait poll =<< recvMsgWait

    serveWait :: ServerStWait v m a -> ClientStWait v m b -> m (a, b)
    serveWait ServerStWait{..} = \case
      SendMsgPoll next -> serveNext next =<< recvMsgPoll
      SendMsgCancel idle -> flip serveIdle idle =<< recvMsgCancel
