{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Protocol.Transfer.Client where

import Control.Monad (join)
import Data.Map (Map)
import Language.Marlowe.Object.Types hiding (Close)
import Language.Marlowe.Protocol.Transfer.Server
import Language.Marlowe.Protocol.Transfer.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.Protocol.Peer.Trace
import Network.TypedProtocol

newtype MarloweTransferClient m a = MarloweTransferClient
  { runMarloweTransferClient :: m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStIdle m a where
  SendMsgTransfer :: ObjectBundle -> ClientStTransfer m a -> ClientStIdle m a
  SendMsgDone :: a -> ClientStIdle m a

deriving instance (Functor m) => Functor (ClientStIdle m)

data ClientStTransfer m a = ClientStTransfer
  { recvMsgTransferred :: Map Label DatumHash -> m (ClientStIdle m a)
  , recvMsgTransferFailed :: TransferError -> m a
  }
  deriving (Functor)

hoistMarloweTransferClient
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweTransferClient m a
  -> MarloweTransferClient n a
hoistMarloweTransferClient f = MarloweTransferClient . f . fmap hoistIdle . runMarloweTransferClient
  where
    hoistIdle :: ClientStIdle m a -> ClientStIdle n a
    hoistIdle = \case
      SendMsgTransfer bundle next -> SendMsgTransfer bundle $ hoistTransfer next
      SendMsgDone a -> SendMsgDone a

    hoistTransfer :: ClientStTransfer m a -> ClientStTransfer n a
    hoistTransfer ClientStTransfer{..} =
      ClientStTransfer
        { recvMsgTransferred = f . fmap hoistIdle . recvMsgTransferred
        , recvMsgTransferFailed = f . recvMsgTransferFailed
        }

marloweTransferClientPeer
  :: forall m a
   . (Functor m)
  => MarloweTransferClient m a
  -> PeerTraced MarloweTransfer 'AsClient 'StIdle m a
marloweTransferClientPeer = EffectTraced . fmap peerIdle . runMarloweTransferClient
  where
    peerIdle :: ClientStIdle m a -> PeerTraced MarloweTransfer 'AsClient 'StIdle m a
    peerIdle = \case
      SendMsgTransfer bundle ClientStTransfer{..} -> YieldTraced (ClientAgency TokIdle) (MsgTransfer bundle) $
        Call (ServerAgency TokTransfer) \case
          MsgTransferred hashes -> EffectTraced $ peerIdle <$> recvMsgTransferred hashes
          MsgTransferFailed err -> EffectTraced $ DoneTraced TokDone <$> recvMsgTransferFailed err
      SendMsgDone a ->
        YieldTraced (ClientAgency TokIdle) MsgDone $
          Close TokDone a

serveMarloweTransferClient
  :: forall m a b
   . (Monad m)
  => MarloweTransferServer m a
  -> MarloweTransferClient m b
  -> m (a, b)
serveMarloweTransferClient MarloweTransferServer{..} MarloweTransferClient{..} =
  join $ serveIdle <$> runMarloweTransferServer <*> runMarloweTransferClient
  where
    serveIdle :: ServerStIdle m a -> ClientStIdle m b -> m (a, b)
    serveIdle ServerStIdle{..} = \case
      SendMsgTransfer bundle next -> serveTransfer next =<< recvMsgTransfer bundle
      SendMsgDone b -> (,b) <$> recvMsgDone

    serveTransfer :: ClientStTransfer m b -> ServerStTransfer m a -> m (a, b)
    serveTransfer ClientStTransfer{..} = \case
      SendMsgTransferred hash next -> serveIdle next =<< recvMsgTransferred hash
      SendMsgTransferFailed err a -> (a,) <$> recvMsgTransferFailed err
