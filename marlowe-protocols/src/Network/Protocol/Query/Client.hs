{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

-- | A generc client for the query protocol. Includes a function for
-- interpreting a client as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Query.Client
  where

import Data.Void (Void)
import Network.Protocol.Query.Types
import Network.TypedProtocol

-- | A generic client for the query protocol.
newtype QueryClient query m a = QueryClient { runQueryClient :: m (ClientStInit query m a) }

-- | In the 'StInit' state, the client has agency. It can send:
--
-- * A request message
data ClientStInit query m a where
  -- | Request the results of a query.
  SendMsgRequest
    :: query delimiter err results
    -> ClientStNextCanReject delimiter err results m a
    -> ClientStInit query m a

deriving instance Functor m => Functor (ClientStInit query m)

-- | In the 'StNext CanReject' state, the server has agency. The client must be prepared
-- to handle either:
--
-- * A next page message with a page of results
-- * A rejection message with an error
data ClientStNextCanReject delimiter err results m a = ClientStNextCanReject
  { recvMsgReject   :: err -> m a
  , recvMsgNextPage :: results -> Maybe delimiter -> m (ClientStPage delimiter err results m a)
  }

deriving instance Functor m => Functor (ClientStNextCanReject delimiter err results m)

-- | In the 'StPage' state, the client has agency. It can send either:
--
-- * A request next message
-- * A done message
data ClientStPage delimiter err results m a where
  -- | Request the next page of results..
  SendMsgRequestNext :: delimiter -> ClientStNext delimiter err results m a -> ClientStPage delimiter err results m a

  -- | Exit the reading loop.
  SendMsgDone :: a -> ClientStPage delimiter err results m a

deriving instance Functor m => Functor (ClientStPage delimiter err results m)

-- | In the 'StNext MustReply state, the server has agency. The client must be prepared
-- to handle
--
-- * A next page message with a page of results
newtype ClientStNext delimiter err results m a = ClientStNext
  { recvMsgNextPage :: results -> Maybe delimiter -> m (ClientStPage delimiter err results m a)
  }

deriving instance Functor m => Functor (ClientStNext delimiter err results m)

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistQueryClient
  :: forall query m n a
   . Functor m
  => (forall x. m x -> n x)
  -> QueryClient query m a
  -> QueryClient query n a
hoistQueryClient phi = QueryClient . phi . fmap hoistInit . runQueryClient
  where
  hoistInit = \case
    SendMsgRequest query next   -> SendMsgRequest query $ hoistNextCanReject next

  hoistNextCanReject
    :: ClientStNextCanReject delimiter err results m a
    -> ClientStNextCanReject delimiter err results n a
  hoistNextCanReject ClientStNextCanReject{..} = ClientStNextCanReject
    { recvMsgReject = phi . recvMsgReject
    , recvMsgNextPage = \results -> phi . fmap hoistPage . recvMsgNextPage results
    }

  hoistPage
    :: ClientStPage delimiter err results m a
    -> ClientStPage delimiter err results n a
  hoistPage = \case
    SendMsgRequestNext delimiter next -> SendMsgRequestNext delimiter $ hoistNext next
    SendMsgDone a                     -> SendMsgDone a

  hoistNext
    :: ClientStNext delimiter err results m a
    -> ClientStNext delimiter err results n a
  hoistNext ClientStNext{..} = ClientStNext
    { recvMsgNextPage = \results -> phi . fmap hoistPage . recvMsgNextPage results
    }

-- | Interpret a client as a typed-protocols peer.
queryClientPeer
  :: forall query m a
   . (Monad m, IsQuery query)
  => QueryClient query m a
  -> Peer (Query query) 'AsClient 'StInit m a
queryClientPeer QueryClient{..} =
  Effect $ peerInit <$> runQueryClient
  where
  peerInit :: ClientStInit query m a -> Peer (Query query) 'AsClient 'StInit m a
  peerInit (SendMsgRequest query next) =
    Yield (ClientAgency TokInit) (MsgRequest query) $ peerNextCanReject (tagFromQuery query) next

  peerNextCanReject
    :: Tag query delimiter err results
    -> ClientStNextCanReject delimiter err results m a
    -> Peer (Query query) 'AsClient ('StNext 'CanReject delimiter err results) m a
  peerNextCanReject tag ClientStNextCanReject{..} =
    Await (ServerAgency (TokNext TokCanReject tag)) $ Effect . \case
      MsgReject err                 -> Done TokDone <$> recvMsgReject err
      MsgNextPage results delimiter -> peerPage tag <$> recvMsgNextPage results delimiter

  peerPage
    :: Tag query delimiter err results
    -> ClientStPage delimiter err results m a
    -> Peer (Query query) 'AsClient ('StPage delimiter err results) m a
  peerPage tag = \case
    SendMsgRequestNext delimiter next ->
      Yield (ClientAgency (TokPage tag)) (MsgRequestNext delimiter) $ peerNext tag next
    SendMsgDone a  -> Yield (ClientAgency (TokPage tag)) MsgDone $ Done TokDone a

  peerNext
    :: Tag query delimiter err results
    -> ClientStNext delimiter err results m a
    -> Peer (Query query) 'AsClient ('StNext 'MustReply delimiter err results) m a
  peerNext query ClientStNext{..} =
    Await (ServerAgency (TokNext TokMustReply query)) $ Effect . \case
      MsgNextPage results delimiter -> peerPage query <$> recvMsgNextPage results delimiter

-- | Create a client that runs a query that cannot have multiple pages.
liftQuery :: Monad m => query Void err results -> QueryClient query m (Either err results)
liftQuery = QueryClient . pure . ($ next) . SendMsgRequest
  where
    next = ClientStNextCanReject
      { recvMsgNextPage = const . pure . SendMsgDone . Right
      , recvMsgReject = pure . Left
      }

-- | Create a client that runs a query that can have multiple pages by
-- enumerating through all the pages and appending the results.
liftFoldQuery :: (Monad m, Monoid results) => query delimiter err results -> QueryClient query m (Either err results)
liftFoldQuery = QueryClient . pure . ($ next) . SendMsgRequest
  where
    next = ClientStNextCanReject
      { recvMsgNextPage = handleNext mempty
      , recvMsgReject = pure . Left
      }
    handleNext agg results Nothing = pure $ SendMsgDone $ Right $ agg <> results
    handleNext agg results (Just d) = pure $ SendMsgRequestNext d ClientStNext
      { recvMsgNextPage = handleNext (agg <> results)
      }
