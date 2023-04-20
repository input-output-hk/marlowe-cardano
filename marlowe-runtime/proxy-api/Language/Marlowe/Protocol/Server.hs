{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Server
  where

import Data.Proxy (Proxy(Proxy))
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as Header
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as Sync
import Language.Marlowe.Protocol.Types
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (hoistDriver)
import Network.Protocol.Handshake.Types (Handshake, signature)
import qualified Network.Protocol.Handshake.Types as Handshake
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.TypedProtocol

data MarloweRuntimeServer m a = forall dState. MarloweRuntimeServer
  { getMarloweSyncDriver :: m (Driver (Handshake MarloweSync) dState m)
  , getMarloweHeaderSyncDriver :: m (Driver (Handshake MarloweHeaderSync) dState m)
  , getMarloweQueryDriver :: m (Driver (Handshake MarloweQuery) dState m)
  , getTxJobDriver :: m (Driver (Handshake (Job MarloweTxCommand)) dState m)
  , result :: a
  }

hoistMarloweRuntimeServer :: Functor m => (forall x. m x -> n x) -> MarloweRuntimeServer m a -> MarloweRuntimeServer n a
hoistMarloweRuntimeServer f MarloweRuntimeServer{..} = MarloweRuntimeServer
  { getMarloweSyncDriver = f $ hoistDriver f <$> getMarloweSyncDriver
  , getMarloweHeaderSyncDriver = f $ hoistDriver f <$> getMarloweHeaderSyncDriver
  , getMarloweQueryDriver = f $ hoistDriver f <$> getMarloweQueryDriver
  , getTxJobDriver = f $ hoistDriver f <$> getTxJobDriver
  , ..
  }

marloweRuntimeServerPeer :: Monad m => MarloweRuntimeServer m a -> Peer MarloweRuntime 'AsServer 'StInit m a
marloweRuntimeServerPeer MarloweRuntimeServer{..} = Await (ClientAgency TokInit) \case
  MsgRunMarloweSync -> result <$ withHandshake (marloweSyncPeer (ClientAgency Sync.TokInit)) getMarloweSyncDriver
  MsgRunMarloweHeaderSync -> result <$ withHandshake (marloweHeaderSyncPeer (ClientAgency Header.TokIdle)) getMarloweHeaderSyncDriver
  MsgRunMarloweQuery -> result <$ withHandshake (marloweQueryPeer (ClientAgency Query.TokReq)) getMarloweQueryDriver
  MsgRunTxJob -> result <$ withHandshake (jobPeer (ClientAgency Job.TokInit)) getTxJobDriver

withHandshake
  :: forall ps dState st m a
   . (Monad m, Handshake.HasSignature ps)
  => (dState -> Driver ps dState m -> Peer MarloweRuntime 'AsServer st m a)
  -> m (Driver (Handshake ps) dState m)
  -> Peer MarloweRuntime 'AsServer st m a
withHandshake main getDriver = Effect do
  driver <- getDriver
  sendMessage driver (ClientAgency Handshake.TokInit) $ Handshake.MsgHandshake $ signature $ Proxy @ps
  (SomeMessage handshakeResponse, dState') <- recvMessage driver (ServerAgency Handshake.TokHandshake) (startDState driver)
  case handshakeResponse of
    Handshake.MsgReject -> error "Handshake rejected by upstream server"
    Handshake.MsgAccept -> pure $ main dState' $ case driver of
      Driver{..} -> Driver
        { sendMessage = \case
            ClientAgency tok -> \msg -> sendMessage (ClientAgency $ Handshake.TokLiftClient tok) $ Handshake.MsgLift msg
            ServerAgency tok -> \msg -> sendMessage (ServerAgency $ Handshake.TokLiftServer tok) $ Handshake.MsgLift msg
        , recvMessage = \case
            ClientAgency tok -> \dState -> do
              (SomeMessage msg, dState'') <- recvMessage (ClientAgency $ Handshake.TokLiftClient tok) dState
              case msg of
                Handshake.MsgLift msg' -> pure (SomeMessage msg', dState'')
            ServerAgency tok -> \dState -> do
              (SomeMessage msg, dState'') <- recvMessage (ServerAgency $ Handshake.TokLiftServer tok) dState
              case msg of
                Handshake.MsgLift msg' -> pure (SomeMessage msg', dState'')
        , ..
        }

marloweSyncPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweSync dState m
  -> Peer MarloweRuntime 'AsServer ('StMarloweSync st) m ()
marloweSyncPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientMarloweSync tok') \case
    MsgMarloweSync msg -> Effect do
      sendMessage driver tok msg
      pure case (tok', msg) of
        (_, Sync.MsgFollowContract{}) -> marloweSyncPeer (ServerAgency Sync.TokFollow) dState driver
        (_, Sync.MsgIntersect _ v _) -> marloweSyncPeer (ServerAgency $ Sync.TokIntersect v) dState driver
        (_, Sync.MsgDone{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()
        (Sync.TokIdle v, Sync.MsgRequestNext{}) -> marloweSyncPeer (ServerAgency $ Sync.TokNext v) dState driver
        (Sync.TokWait v, Sync.MsgPoll{}) -> marloweSyncPeer (ServerAgency $ Sync.TokNext v) dState driver
        (Sync.TokWait v, Sync.MsgCancel{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState driver
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerMarloweSync tok') (MsgMarloweSync msg) case (tok', msg) of
      (_, Sync.MsgContractNotFound{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()
      (_, Sync.MsgContractFound _ v _) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (Sync.TokNext v, Sync.MsgRollForward{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (Sync.TokNext v, Sync.MsgRollBackward{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgRollBackCreation{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()
      (Sync.TokNext v, Sync.MsgWait{}) -> marloweSyncPeer (ClientAgency $ Sync.TokWait v) dState' driver
      (Sync.TokIntersect v, Sync.MsgIntersectFound{}) -> marloweSyncPeer (ClientAgency $ Sync.TokIdle v) dState' driver
      (_, Sync.MsgIntersectNotFound{}) -> Done (TokNobodyMarloweSync Sync.TokDone) ()

marloweHeaderSyncPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweHeaderSync dState m
  -> Peer MarloweRuntime 'AsServer ('StMarloweHeaderSync st) m ()
marloweHeaderSyncPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientMarloweHeaderSync tok') \case
    MsgMarloweHeaderSync msg -> Effect do
      sendMessage driver tok msg
      pure case msg of
        Header.MsgIntersect{} -> marloweHeaderSyncPeer (ServerAgency Header.TokIntersect) dState driver
        Header.MsgDone{} -> Done (TokNobodyMarloweHeaderSync Header.TokDone) ()
        Header.MsgRequestNext{} -> marloweHeaderSyncPeer (ServerAgency Header.TokNext) dState driver
        Header.MsgPoll{} -> marloweHeaderSyncPeer (ServerAgency Header.TokNext) dState driver
        Header.MsgCancel{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState driver
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerMarloweHeaderSync tok') (MsgMarloweHeaderSync msg) case msg of
      Header.MsgNewHeaders{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver
      Header.MsgRollBackward{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver
      Header.MsgWait{} -> marloweHeaderSyncPeer (ClientAgency Header.TokWait) dState' driver
      Header.MsgIntersectFound{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver
      Header.MsgIntersectNotFound{} -> marloweHeaderSyncPeer (ClientAgency Header.TokIdle) dState' driver

marloweQueryPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver MarloweQuery dState m
  -> Peer MarloweRuntime 'AsServer ('StMarloweQuery st) m ()
marloweQueryPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientMarloweQuery tok') \case
    MsgMarloweQuery msg -> Effect do
      sendMessage driver tok msg
      pure case msg of
        Query.MsgRequest req -> marloweQueryPeer (ServerAgency $ Query.TokRes $ Query.requestToSt req) dState driver
        Query.MsgDone -> Done (TokNobodyMarloweQuery Query.TokDone) ()
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerMarloweQuery tok') (MsgMarloweQuery msg) case msg of
      Query.MsgRespond{} -> marloweQueryPeer (ClientAgency Query.TokReq) dState' driver

jobPeer
  :: Monad m
  => PeerHasAgency pr st
  -> dState
  -> Driver (Job MarloweTxCommand) dState m
  -> Peer MarloweRuntime 'AsServer ('StTxJob st) m ()
jobPeer tok dState driver = case tok of
  ClientAgency tok' -> Await (ClientAgency $ TokClientTxJob tok') \case
    MsgTxJob msg -> Effect do
      sendMessage driver tok msg
      pure case (tok', msg) of
        (_, Job.MsgExec cmd) -> jobPeer (ServerAgency $ Job.TokCmd $ Job.tagFromCommand cmd) dState driver
        (_, Job.MsgAttach jobId) -> jobPeer (ServerAgency $ Job.TokAttach $ Job.tagFromJobId jobId) dState driver
        (Job.TokAwait tag, Job.MsgPoll) -> jobPeer (ServerAgency $ Job.TokCmd tag) dState driver
        (_, Job.MsgDetach) -> Done (TokNobodyTxJob Job.TokDone) ()
  ServerAgency tok' -> Effect do
    (SomeMessage msg, dState') <- recvMessage driver tok dState
    pure $ Yield (ServerAgency $ TokServerTxJob tok') (MsgTxJob msg) case (tok', msg) of
      (_, Job.MsgFail{}) -> Done (TokNobodyTxJob Job.TokDone) ()
      (_, Job.MsgSucceed{}) -> Done (TokNobodyTxJob Job.TokDone) ()
      (Job.TokCmd tag, Job.MsgAwait{}) -> jobPeer (ClientAgency $ Job.TokAwait tag) dState' driver
      (Job.TokAttach tag, Job.MsgAttached{}) -> jobPeer (ServerAgency $ Job.TokCmd tag) dState' driver
      (_, Job.MsgAttachFailed{}) -> Done (TokNobodyTxJob Job.TokDone) ()
