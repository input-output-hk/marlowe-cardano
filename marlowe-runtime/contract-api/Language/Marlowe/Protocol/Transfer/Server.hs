{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Transfer.Server where

import Data.Functor ((<&>))
import Data.Map (Map)
import Language.Marlowe.Object.Types hiding (Close)
import Language.Marlowe.Protocol.Transfer.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

newtype MarloweTransferServer m a = MarloweTransferServer
  { runtMarloweTransferServer :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStIdle m a = ServerStIdle
  { recvMsgTransfer :: ObjectBundle -> m (ServerStTransfer m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

data ServerStTransfer m a where
  SendMsgTransferred :: Map Label DatumHash -> ServerStIdle m a -> ServerStTransfer m a
  SendMsgTransferFailed :: LinkError -> a -> ServerStTransfer m a

deriving instance (Functor m) => Functor (ServerStTransfer m)

hoistMarloweTransferServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweTransferServer m a
  -> MarloweTransferServer n a
hoistMarloweTransferServer f = MarloweTransferServer . f . fmap hoistIdle . runtMarloweTransferServer
  where
    hoistIdle :: ServerStIdle m a -> ServerStIdle n a
    hoistIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgTransfer = f . fmap hoistTransfer . recvMsgTransfer
        , recvMsgDone = f recvMsgDone
        }

    hoistTransfer :: ServerStTransfer m a -> ServerStTransfer n a
    hoistTransfer = \case
      SendMsgTransferred hashes next -> SendMsgTransferred hashes $ hoistIdle next
      SendMsgTransferFailed err a -> SendMsgTransferFailed err a

marloweTransferServerPeer
  :: forall m a
   . (Functor m)
  => MarloweTransferServer m a
  -> PeerTraced MarloweTransfer 'AsServer 'StIdle m a
marloweTransferServerPeer = EffectTraced . fmap peerIdle . runtMarloweTransferServer
  where
    peerIdle :: ServerStIdle m a -> PeerTraced MarloweTransfer 'AsServer 'StIdle m a
    peerIdle ServerStIdle{..} = AwaitTraced (ClientAgency TokIdle) \case
      MsgTransfer bundle ->
        Respond (ServerAgency TokTransfer) $
          recvMsgTransfer bundle <&> \case
            SendMsgTransferred hashes idle -> Response (MsgTransferred hashes) $ peerIdle idle
            SendMsgTransferFailed err a -> Response (MsgTransferFailed err) $ DoneTraced TokDone a
      MsgDone -> Closed TokDone recvMsgDone
