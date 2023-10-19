{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic client for the query protocol. Includes a function for
-- interpreting a client as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Query.Client where

import Control.Applicative (liftA2)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import Network.Protocol.Peer.Monad (ClientT)
import qualified Network.Protocol.Peer.Monad as PeerT
import Network.Protocol.Query.Types

-- | A generic client for the query protocol.
data QueryClient k m a where
  ClientRequest :: Request (t :: Tree k) -> (Response t -> m (QueryClient k m a)) -> QueryClient k m a
  ClientLift :: m (QueryClient k m a) -> QueryClient k m a
  ClientPure :: a -> QueryClient k m a

deriving instance (Functor m) => Functor (QueryClient req m)

instance (Applicative m) => Applicative (QueryClient req m) where
  pure = ClientPure
  ClientPure f <*> a = f <$> a
  f <*> ClientPure a = ($ a) <$> f
  f <*> ClientLift a = ClientLift $ (f <*>) <$> a
  ClientLift f <*> a = ClientLift $ (<*> a) <$> f
  ClientRequest req1 contF <*> ClientRequest req2 contA =
    ClientRequest (ReqBin req1 req2) \(ResBin r1 r2) -> liftA2 (<*>) (contF r1) (contA r2)

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

request :: (Applicative m) => Request (t :: k) -> QueryClient k m (Response t)
request req = ClientRequest (ReqLeaf req) $ pure . pure . \case ResLeaf t -> t

runQueryClient
  :: forall k m a
   . (Monad m, TagKind k)
  => QueryClient k m a
  -> ClientT (Query (Tree k)) 'StReq 'StDone m a
runQueryClient client = PeerT.do
  a <- go client
  PeerT.yield MsgDone
  pure a
  where
    go :: QueryClient k m x -> ClientT (Query (Tree k)) 'StReq 'StReq m x
    go = \case
      ClientPure a -> pure a
      ClientLift m -> go =<< lift m
      ClientRequest req cont -> withSingTag (requestTag req) PeerT.do
        PeerT.yield (MsgRequest req)
        PeerT.await \case
          MsgRespond r -> go =<< lift (cont r)
