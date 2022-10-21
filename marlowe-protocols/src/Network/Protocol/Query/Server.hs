{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- | A generc server for the query protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Query.Server
  where

import Network.Protocol.Query.Types
import Network.Protocol.SchemaVersion (SchemaVersion)
import Network.TypedProtocol

-- | A generic server for the query protocol.
newtype QueryServer query m a = QueryServer { runQueryServer :: m (ServerStInit query m a) }

-- | In the 'StInit' protocol state, the server does not have agency. Instead,
-- it is waiting to handle a handshake request from the client, which it must
-- handle.
newtype ServerStInit query m a = ServerStInit
  { recvMsgRequestHandshake :: SchemaVersion query -> m (ServerStHandshake query m a)
  }

-- | In the 'StHandshake' protocol state, the server has agency. It must send
-- either:
--
-- * a handshake rejection message
-- * a handshake confirmation message
data ServerStHandshake cmd m a where

  -- | Reject the handshake request
  SendMsgHandshakeRejected
    :: SchemaVersion cmd -- ^ A supported schema version.
    -> a                -- ^ The result of running the protocol.
    -> ServerStHandshake cmd m a

  -- | Accept the handshake request
  SendMsgHandshakeConfirmed
    :: m (ServerStIdle cmd m a) -- ^ An action that computes the idle handlers.
    -> ServerStHandshake cmd m a

-- | In the 'StInit' state, the client has agency. The server must be prepared
-- to handle:
--
-- * A request message
data ServerStIdle query m a = ServerStIdle
    { recvMsgRequest
        :: forall delimiter err results
         . query delimiter err results
        -> m (ServerStNext query 'CanReject delimiter err results m a)
    , recvMsgDone        :: m a
    } deriving Functor

-- | In the 'StNext' state, the server has agency. It can send either:
--
-- * A next page message with a page of results
-- * A rejection message with an error (as long as no previous pages have been sent.
data ServerStNext query (k :: StNextKind) delimiter err results m a where
  SendMsgReject
    :: err
    -> a
    -> ServerStNext query 'CanReject delimiter err results m a
  SendMsgNextPage
    :: results
    -> Maybe delimiter
    -> ServerStPage query delimiter err results m a
    -> ServerStNext query k delimiter err results m a

deriving instance Functor m => Functor (ServerStNext query k delimiter err results m)

-- | In the 'StPage' state, the client has agency. The server must be prepared
-- to handle either:
--
-- * A request next message
-- * A done message
data ServerStPage query delimiter err results m a = ServerStPage
  { recvMsgRequestNext :: delimiter -> m (ServerStNext query 'MustReply delimiter err results m a)
  , recvMsgRequestDone        :: m a
  } deriving Functor

-- | Change the underlying monad type a server runs in with a natural
-- transformation.
hoistQueryServer
  :: forall query m n a
   . Functor m
  => (forall x. m x -> n x)
  -> QueryServer query m a
  -> QueryServer query n a
hoistQueryServer phi = QueryServer . phi . fmap hoistInit . runQueryServer
  where
  hoistInit
    :: ServerStInit query m a
    -> ServerStInit query n a
  hoistInit = ServerStInit . fmap (phi . fmap hoistHandshake) . recvMsgRequestHandshake

  hoistHandshake
    :: ServerStHandshake query m a
    -> ServerStHandshake query n a
  hoistHandshake (SendMsgHandshakeRejected vs a)  = SendMsgHandshakeRejected vs a
  hoistHandshake (SendMsgHandshakeConfirmed idle) = SendMsgHandshakeConfirmed $ phi $ hoistIdle <$> idle

  hoistIdle ServerStIdle{..} = ServerStIdle
    { recvMsgRequest = phi . fmap hoistNextCanReject . recvMsgRequest
    , recvMsgDone = phi recvMsgDone
    }

  hoistNextCanReject
    :: ServerStNext query k delimiter err results m a
    -> ServerStNext query k delimiter err results n a
  hoistNextCanReject = \case
    SendMsgReject err a                    -> SendMsgReject err a
    SendMsgNextPage results delimiter page -> SendMsgNextPage results delimiter $ hoistPage page

  hoistPage
    :: ServerStPage query delimiter err results m a
    -> ServerStPage query delimiter err results n a
  hoistPage ServerStPage{..} = ServerStPage
    { recvMsgRequestNext = phi . fmap hoistNextCanReject . recvMsgRequestNext
    , recvMsgRequestDone = phi recvMsgRequestDone
    }

-- | Interpret a server as a typed-protocols peer.
queryServerPeer
  :: forall query m a
   . (Monad m, IsQuery query)
  => QueryServer query m a
  -> Peer (Query query) 'AsServer 'StInit m a
queryServerPeer QueryServer{..} =
  Effect $ peerInit <$> runQueryServer
  where
  peerInit
    :: ServerStInit query m a
    -> Peer (Query query) 'AsServer 'StInit m a
  peerInit ServerStInit{..} = Await (ClientAgency TokInit) \case
    MsgRequestHandshake version -> Effect $ peerHandshake <$> recvMsgRequestHandshake version

  peerHandshake
    :: ServerStHandshake query m a
    -> Peer (Query query) 'AsServer 'StHandshake m a
  peerHandshake = \case
    SendMsgHandshakeRejected version ma ->
      Yield (ServerAgency TokHandshake) (MsgRejectHandshake version) $
      Done TokFault ma
    SendMsgHandshakeConfirmed serverIdle ->
      Yield (ServerAgency TokHandshake) MsgConfirmHandshake $
      peerIdle serverIdle

  peerIdle
    :: m (ServerStIdle query m a)
    -> Peer (Query query) 'AsServer 'StIdle m a
  peerIdle = Effect . fmap peerIdle_

  peerIdle_ :: ServerStIdle query m a -> Peer (Query query) 'AsServer 'StIdle m a
  peerIdle_ ServerStIdle{..} =
    Await (ClientAgency TokIdle) \case
      (MsgRequest query) ->
        peerNext TokCanReject (tagFromQuery query) $ recvMsgRequest query
      MsgDone -> Effect $ Done TokDone <$> recvMsgDone

  peerNext
    :: TokNextKind k
    -> Tag query delimiter err results
    -> m (ServerStNext query k delimiter err results m a)
    -> Peer (Query query) 'AsServer ('StNext k delimiter err results) m a
  peerNext tok tag = Effect . fmap (peerNext_ tok tag)

  peerNext_
    :: forall k delimiter err results
     . TokNextKind k
    -> Tag query delimiter err results
    -> ServerStNext query k delimiter err results m a
    -> Peer (Query query) 'AsServer ('StNext k delimiter err results) m a
  peerNext_ k tag = \case
    SendMsgReject err a ->
      Yield (ServerAgency (TokNext k tag)) (MsgReject err) $ Done TokDone a
    SendMsgNextPage results delimiter page ->
      Yield (ServerAgency (TokNext k tag)) (MsgNextPage results delimiter) $ peerPage tag page

  peerPage
    :: Tag query delimiter err results
    -> ServerStPage query delimiter err results m a
    -> Peer (Query query) 'AsServer ('StPage delimiter err results) m a
  peerPage tag ServerStPage{..} =
    Await (ClientAgency (TokPage tag)) \case
      MsgRequestNext delimiter -> peerNext TokMustReply tag $ recvMsgRequestNext delimiter
      MsgRequestDone -> Effect $ Done TokDone <$> recvMsgRequestDone

queryServer
  :: Applicative m
  => SchemaVersion query
  -> m (ServerStIdle query m ())
  -> QueryServer query m ()
queryServer version stIdle = QueryServer $ do
  pure $ ServerStInit \version' -> pure if version == version'
      then SendMsgHandshakeConfirmed stIdle
      else SendMsgHandshakeRejected version ()

-- | Create a server that does not return results in multiple pages. Requesting
-- next will always return the same results as the first page.
liftHandler
  :: forall query m
   . Monad m
  => SchemaVersion query
  -> (forall delimiter err results. query delimiter err results -> m (Either err results))
  -> QueryServer query m ()
liftHandler version handle = queryServer version $ pure $ ServerStIdle
  { recvMsgRequest = \query -> do
      result <- handle query
      pure case result of
        Left err      -> SendMsgReject err ()
        Right results -> sendResults results
  , recvMsgDone = pure ()
  }
  where
    sendResults :: results -> ServerStNext query k delimiter err results m ()
    sendResults results = SendMsgNextPage results Nothing ServerStPage
      { recvMsgRequestDone = pure ()
      , recvMsgRequestNext = \_ -> pure $ sendResults results
      }

