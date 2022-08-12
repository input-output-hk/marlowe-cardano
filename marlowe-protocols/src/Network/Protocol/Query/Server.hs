{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}

-- | A generc server for the query protocol. Includes a function for
-- interpreting a server as a typed-protocols peer that can be executed with a
-- driver and a codec.

module Network.Protocol.Query.Server where

import Network.Protocol.Query.Types
import Network.TypedProtocol

-- | A generic server for the query protocol.
newtype QueryServer query m a = QueryServer { runQueryServer :: m (ServerStInit query m a) }

-- | In the 'StInit' state, the client has agency. The server must be prepared
-- to handle:
--
-- * A request message
newtype ServerStInit query m a = ServerStInit
    { recvMsgRequest
        :: forall delimiter err results
         . query delimiter err results
        -> m (ServerStNext query 'CanReject delimiter err results m a)
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
  , recvMsgDone        :: m a
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
  hoistInit ServerStInit{..} = ServerStInit
    { recvMsgRequest = phi . fmap hoistNextCanReject . recvMsgRequest
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
    , recvMsgDone = phi recvMsgDone
    }

-- | Interpret a server as a typed-protocols peer.
queryServerPeer
  :: forall query m a
   . Monad m
  => QueryServer query m a
  -> Peer (Query query) 'AsServer 'StInit m a
queryServerPeer QueryServer{..} =
  Effect $ peerInit <$> runQueryServer
  where
  peerInit :: ServerStInit query m a -> Peer (Query query) 'AsServer 'StInit m a
  peerInit ServerStInit{..} =
    Await (ClientAgency TokInit) \(MsgRequest query) ->
      peerNext TokCanReject query $ recvMsgRequest query

  peerNext
    :: TokNextKind k
    -> query delimiter err results
    -> m (ServerStNext query k delimiter err results m a)
    -> Peer (Query query) 'AsServer ('StNext k delimiter err results) m a
  peerNext tok query = Effect . fmap (peerNext_ tok query)

  peerNext_
    :: forall k delimiter err results
     . TokNextKind k
    -> query delimiter err results
    -> ServerStNext query k delimiter err results m a
    -> Peer (Query query) 'AsServer ('StNext k delimiter err results) m a
  peerNext_ k query = \case
    SendMsgReject err a ->
      Yield (ServerAgency (TokNext k query)) (MsgReject query err) $ Done TokDone a
    SendMsgNextPage results delimiter page ->
      Yield (ServerAgency (TokNext k query)) (MsgNextPage query results delimiter) $ peerPage query page

  peerPage
    :: query delimiter err results
    -> ServerStPage query delimiter err results m a
    -> Peer (Query query) 'AsServer ('StPage delimiter err results) m a
  peerPage query ServerStPage{..} =
    Await (ClientAgency (TokPage query)) \case
      MsgRequestNext _ delimiter -> peerNext TokMustReply query $ recvMsgRequestNext delimiter
      MsgDone                    -> Effect $ Done TokDone <$> recvMsgDone

-- | Create a server that does not return results in multiple pages. Requesting
-- next will always return the same results as the first page.
liftHandler
  :: forall query m a
   . Monad m
  => (forall delimiter err results. query delimiter err results -> m (a, Either err results))
  -> QueryServer query m a
liftHandler handle = QueryServer $ pure $ ServerStInit \query -> do
  (a, result) <- handle query
  pure case result of
    Left err      -> SendMsgReject err a
    Right results -> sendResults a results
  where
    sendResults :: a -> results -> ServerStNext query k delimiter err results m a
    sendResults a results = SendMsgNextPage results Nothing ServerStPage
      { recvMsgDone = pure a
      , recvMsgRequestNext = \_ -> pure $ sendResults a results
      }
