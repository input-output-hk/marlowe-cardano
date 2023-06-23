{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the query protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Query.Server where

import Control.Arrow ((***))
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Types
import Network.TypedProtocol

-- | A generic server for the query protocol.
newtype QueryServer req m a = QueryServer {runQueryServer :: m (ServerStReq req m a)}
  deriving (Functor)

data ServerStReq req m a = ServerStReq
  { recvMsgRequest :: forall x. ReqTree req x -> m (x, ServerStReq req m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistQueryServer
  :: forall req m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> QueryServer req m a
  -> QueryServer req n a
hoistQueryServer f = QueryServer . f . fmap hoistReq . runQueryServer
  where
    hoistReq ServerStReq{..} =
      ServerStReq
        { recvMsgRequest = f . (fmap . fmap) hoistReq . recvMsgRequest
        , recvMsgDone = f recvMsgDone
        }

-- | Interpret a server as a typed-protocols peer.
queryServerPeer
  :: forall req m a
   . (Functor m, Request req)
  => QueryServer req m a
  -> PeerTraced (Query req) 'AsServer 'StReq m a
queryServerPeer QueryServer{..} =
  EffectTraced $ peerReq <$> runQueryServer
  where
    peerReq :: ServerStReq req m a -> PeerTraced (Query req) 'AsServer 'StReq m a
    peerReq ServerStReq{..} =
      AwaitTraced (ClientAgency TokReq) \case
        MsgRequest req ->
          Respond (ServerAgency $ TokRes $ tagFromReq req) $
            uncurry Response . (MsgRespond *** peerReq) <$> recvMsgRequest req
        MsgDone -> Closed TokDone recvMsgDone

respond
  :: forall req m
   . (Applicative m)
  => (forall a b. m a -> m b -> m (a, b))
  -> (forall a. req a -> m a)
  -> QueryServer req m ()
respond merge handle = QueryServer $ pure server
  where
    server =
      ServerStReq
        { recvMsgDone = pure ()
        , recvMsgRequest = fmap (,server) . handle'
        }
    handle' :: ReqTree req a -> m a
    handle' = \case
      ReqLeaf req -> handle req
      ReqBin l r -> handle' l `merge` handle' r
