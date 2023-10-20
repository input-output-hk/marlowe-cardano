{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the query protocol. Includes a function for
-- interpreting a client as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Query.Client where

import Control.Applicative (liftA2)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Network.Protocol.Peer.Trace
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Network.TypedProtocol

-- | A generic client for the query protocol.
data QueryClient req m a where
  ClientRequest :: ReqTree req x -> (x -> m (QueryClient req m a)) -> QueryClient req m a
  ClientLift :: m (QueryClient req m a) -> QueryClient req m a
  ClientPure :: a -> QueryClient req m a

deriving instance (Functor m) => Functor (QueryClient req m)

instance (Applicative m) => Applicative (QueryClient req m) where
  pure = ClientPure
  ClientPure f <*> a = f <$> a
  f <*> ClientPure a = ($ a) <$> f
  f <*> ClientLift a = ClientLift $ (f <*>) <$> a
  ClientLift f <*> a = ClientLift $ (<*> a) <$> f
  ClientRequest req1 contF <*> ClientRequest req2 contA =
    ClientRequest (ReqBin req1 req2) \(r1, r2) -> liftA2 (<*>) (contF r1) (contA r2)

instance (Monad m) => Monad (QueryClient req m) where
  ClientPure a >>= k = k a
  ClientLift m >>= k = ClientLift $ (>>= k) <$> m
  ClientRequest req cont >>= k = ClientRequest req $ fmap (>>= k) . cont

instance MonadTrans (QueryClient req) where
  lift = ClientLift . fmap pure

instance (MonadIO m) => MonadIO (QueryClient req m) where
  liftIO = lift . liftIO

hoistQueryClient :: (Functor m) => (forall x. m x -> n x) -> QueryClient req m a -> QueryClient req n a
hoistQueryClient f = \case
  ClientPure a -> ClientPure a
  ClientLift m -> ClientLift $ f $ hoistQueryClient f <$> m
  ClientRequest req cont -> ClientRequest req $ f . fmap (hoistQueryClient f) . cont

request :: (Applicative m) => req a -> QueryClient req m a
request req = ClientRequest (ReqLeaf req) $ pure . pure

queryClientPeer
  :: (Functor m, Request req)
  => QueryClient req m a
  -> PeerTraced (Query req) 'AsClient 'StReq m a
queryClientPeer = \case
  ClientPure a ->
    YieldTraced (ClientAgency TokReq) MsgDone $
      Close TokDone a
  ClientLift m ->
    EffectTraced $ queryClientPeer <$> m
  ClientRequest req cont ->
    YieldTraced (ClientAgency TokReq) (MsgRequest req) $
      Call (ServerAgency (TokRes $ tagFromReq req)) \case
        MsgRespond r -> EffectTraced $ queryClientPeer <$> cont r

serveQueryClient
  :: forall req m a b
   . (Monad m)
  => QueryServer req m a
  -> QueryClient req m b
  -> m (a, b)
serveQueryClient QueryServer{..} client = join $ serveReq <$> runQueryServer <*> pure client
  where
    serveReq :: ServerStReq req m a -> QueryClient req m b -> m (a, b)
    serveReq server@ServerStReq{..} = \case
      ClientRequest reqTree k -> do
        (x, next) <- recvMsgRequest reqTree
        serveReq next =<< k x
      ClientLift m -> serveReq server =<< m
      ClientPure b -> (,b) <$> recvMsgDone
