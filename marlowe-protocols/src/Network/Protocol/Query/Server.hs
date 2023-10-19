{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE RankNTypes #-}

-- | A generic server for the query protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.
module Network.Protocol.Query.Server where

import Control.Monad.Trans (MonadTrans (..))
import Network.Protocol.Peer.Monad (ServerT)
import qualified Network.Protocol.Peer.Monad as PeerT
import Network.Protocol.Query.Types

type QueryServerT k = ServerT (Query k) 'StReq 'StDone

-- | Lift a handler function into a query server which responds until the client
-- terminates the session.
respond
  :: (Monad m, TagKind k)
  => (forall (t :: k). Request t -> m (Response t))
  -> QueryServerT k m ()
respond handle = go
  where
    go = PeerT.await \case
      MsgDone -> pure ()
      MsgRequest req -> withSingTag (requestTag req) $ PeerT.do
        response <- lift $ handle req
        PeerT.yield $ MsgRespond response
        go

-- | Lift a function that handles individual requests into a function that handles
-- trees of requests in an Applicative functor. For example, to handle requests
-- concurrently:
-- @@
-- import UnliftIO (Concurrently(..))
--
-- batchWith Concurrently runConcurrently handle
-- @@
batchWith
  :: forall k m n
   . (Applicative n)
  => (forall a. m a -> n a)
  -> (forall a. n a -> m a)
  -> (forall (t :: k). Request t -> m (Response t))
  -> (forall (t :: Tree k). Request t -> m (Response t))
batchWith wrap unwrap handleLeaf = unwrap . go
  where
    go :: forall (t :: Tree k). Request t -> n (Response t)
    go = \case
      ReqLeaf req -> ResLeaf <$> wrap (handleLeaf req)
      ReqBin l r -> ResBin <$> go l <*> go r
