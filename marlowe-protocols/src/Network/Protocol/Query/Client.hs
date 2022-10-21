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
import Network.Protocol.SchemaVersion (SchemaVersion)
import Network.TypedProtocol

-- | A generic client for the query protocol.
newtype QueryClient query m a = QueryClient { runQueryClient :: m (ClientStInit query m a) }

-- | In the 'StInit' protocol state, the client has agency. It must send a
-- handshake request message.
data ClientStInit query m a
  = SendMsgRequestHandshake (SchemaVersion query) (ClientStHandshake query m a)

-- | In the 'StHandshake' protocol state, the client does not have agency.
-- Instead, it must be prepared to handle either:
--
-- * a handshake rejection message
-- * a handshake confirmation message
data ClientStHandshake cmd m a = ClientStHandshake
  { recvMsgHandshakeRejected  :: SchemaVersion cmd -> m a
  , recvMsgHandshakeConfirmed :: m (ClientStIdle cmd m a)
  }

-- | In the 'StIdle' state, the client has agency. It can send:
--
-- * A request message
data ClientStIdle query m a where
  SendMsgDone :: a -> ClientStIdle query m a
  -- | Request the results of a query.
  SendMsgRequest
    :: query delimiter err results
    -> ClientStNextCanReject delimiter err results m a
    -> ClientStIdle query m a

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
  SendMsgRequestDone :: a -> ClientStPage delimiter err results m a

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
  hoistInit :: ClientStInit query m a -> ClientStInit query n a
  hoistInit (SendMsgRequestHandshake v idle) = SendMsgRequestHandshake v $ hoistHandshake idle

  hoistHandshake :: ClientStHandshake query m a -> ClientStHandshake query n a
  hoistHandshake ClientStHandshake{..} = ClientStHandshake
    { recvMsgHandshakeRejected = phi <$> recvMsgHandshakeRejected
    , recvMsgHandshakeConfirmed = phi $ hoistIdle <$> recvMsgHandshakeConfirmed
    }

  hoistIdle = \case
    SendMsgRequest query next   -> SendMsgRequest query $ hoistNextCanReject next
    SendMsgDone a -> SendMsgDone a

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
    SendMsgRequestDone a -> SendMsgRequestDone a

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
  peerInit
    :: ClientStInit query m a
    -> Peer (Query query) 'AsClient 'StInit m a
  peerInit (SendMsgRequestHandshake schemaVersion handshake) =
    Yield (ClientAgency TokInit) (MsgRequestHandshake schemaVersion) $ peerHandshake handshake

  peerHandshake
    :: ClientStHandshake query m a
    -> Peer (Query query) 'AsClient 'StHandshake m a
  peerHandshake ClientStHandshake{..} =
    Await (ServerAgency TokHandshake) \case
      MsgRejectHandshake version -> Effect $ Done TokFault <$> recvMsgHandshakeRejected version
      MsgConfirmHandshake         -> peerIdle  recvMsgHandshakeConfirmed

  peerIdle
    :: m (ClientStIdle query m a)
    -> Peer (Query query) 'AsClient 'StIdle m a
  peerIdle = Effect . fmap peerIdle_

  peerIdle_
    :: ClientStIdle query m a
    -> Peer (Query query) 'AsClient 'StIdle m a
  peerIdle_ (SendMsgRequest query next) =
    Yield (ClientAgency TokIdle) (MsgRequest query) $ peerNextCanReject (tagFromQuery query) next
  peerIdle_ (SendMsgDone a) =
    Yield (ClientAgency TokIdle) MsgDone $ Done TokDone a

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
    SendMsgRequestDone a  -> Yield (ClientAgency (TokPage tag)) MsgRequestDone $ Done TokDone a

  peerNext
    :: Tag query delimiter err results
    -> ClientStNext delimiter err results m a
    -> Peer (Query query) 'AsClient ('StNext 'MustReply delimiter err results) m a
  peerNext query ClientStNext{..} =
    Await (ServerAgency (TokNext TokMustReply query)) $ Effect . \case
      MsgNextPage results delimiter -> peerPage query <$> recvMsgNextPage results delimiter

queryClient
  :: Monad m
  => SchemaVersion query
  -> m (ClientStHandshake query m a)
  -> QueryClient query m a
queryClient version clientStHandshake =
  QueryClient do
    SendMsgRequestHandshake version <$> clientStHandshake

-- | Create a client that runs a query that cannot have multiple pages.
liftQuery
  :: Monad m
  => SchemaVersion query
  -> query Void err results
  -> QueryClient query m (Either (Either (SchemaVersion query) err) results)
liftQuery version query = queryClient version . pure $ ClientStHandshake
  { recvMsgHandshakeRejected = \version' -> pure . Left . Left $ version'
  , recvMsgHandshakeConfirmed = pure stReq
  }
  where
    stReq = SendMsgRequest query next
    next = ClientStNextCanReject
      { recvMsgNextPage = const . pure . SendMsgRequestDone . Right
      , recvMsgReject = pure . Left . Right
      }

doHandshake
  ::  Monad m
  => SchemaVersion query
  -> QueryClient query m (Either (SchemaVersion query) ())
doHandshake version = queryClient version . pure $ ClientStHandshake
  { recvMsgHandshakeRejected  = \version' -> pure $ Left version'
  , recvMsgHandshakeConfirmed = pure $ SendMsgDone (Right ())
  }
