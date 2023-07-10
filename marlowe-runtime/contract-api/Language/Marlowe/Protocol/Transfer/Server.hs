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
import Numeric.Natural (Natural)

newtype MarloweTransferServer m a = MarloweTransferServer
  { runMarloweTransferServer :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStIdle m a = ServerStIdle
  { recvMsgStartImport :: m (ServerStCanUpload m a)
  , recvMsgRequestExport :: DatumHash -> m (ServerStExport m a)
  , recvMsgDone :: m a
  }
  deriving (Functor)

data ServerStExport m a where
  SendMsgStartExport :: ServerStCanDownload m a -> ServerStExport m a
  SendMsgContractNotFound :: ServerStIdle m a -> ServerStExport m a

deriving instance (Functor m) => Functor (ServerStExport m)

data ServerStCanUpload m a = ServerStCanUpload
  { recvMsgUpload :: ObjectBundle -> m (ServerStUpload m a)
  , recvMsgImported :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStCanDownload m a = ServerStCanDownload
  { recvMsgDownload :: Natural -> m (ServerStDownload m a)
  , recvMsgCancel :: m (ServerStIdle m a)
  }
  deriving (Functor)

data ServerStDownload m a where
  SendMsgDownloaded :: ObjectBundle -> ServerStCanDownload m a -> ServerStDownload m a
  SendMsgExported :: ServerStIdle m a -> ServerStDownload m a

deriving instance (Functor m) => Functor (ServerStDownload m)

data ServerStUpload m a where
  SendMsgUploaded :: Map Label DatumHash -> ServerStCanUpload m a -> ServerStUpload m a
  SendMsgUploadFailed :: ImportError -> ServerStIdle m a -> ServerStUpload m a

deriving instance (Functor m) => Functor (ServerStUpload m)

hoistMarloweTransferServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweTransferServer m a
  -> MarloweTransferServer n a
hoistMarloweTransferServer f = MarloweTransferServer . f . fmap hoistIdle . runMarloweTransferServer
  where
    hoistIdle :: ServerStIdle m a -> ServerStIdle n a
    hoistIdle ServerStIdle{..} =
      ServerStIdle
        { recvMsgStartImport = f $ hoistCanUpload <$> recvMsgStartImport
        , recvMsgRequestExport = f . fmap hoistExport . recvMsgRequestExport
        , recvMsgDone = f recvMsgDone
        }

    hoistExport :: ServerStExport m a -> ServerStExport n a
    hoistExport = \case
      SendMsgStartExport next -> SendMsgStartExport $ hoistCanDownload next
      SendMsgContractNotFound next -> SendMsgContractNotFound $ hoistIdle next

    hoistCanUpload :: ServerStCanUpload m a -> ServerStCanUpload n a
    hoistCanUpload ServerStCanUpload{..} =
      ServerStCanUpload
        { recvMsgUpload = f . fmap hoistUpload . recvMsgUpload
        , recvMsgImported = f $ hoistIdle <$> recvMsgImported
        }

    hoistDownload :: ServerStDownload m a -> ServerStDownload n a
    hoistDownload = \case
      SendMsgDownloaded bundle next -> SendMsgDownloaded bundle $ hoistCanDownload next
      SendMsgExported next -> SendMsgExported $ hoistIdle next

    hoistUpload :: ServerStUpload m a -> ServerStUpload n a
    hoistUpload = \case
      SendMsgUploaded hashes next -> SendMsgUploaded hashes $ hoistCanUpload next
      SendMsgUploadFailed err next -> SendMsgUploadFailed err $ hoistIdle next

    hoistCanDownload :: ServerStCanDownload m a -> ServerStCanDownload n a
    hoistCanDownload ServerStCanDownload{..} =
      ServerStCanDownload
        { recvMsgDownload = f . fmap hoistDownload . recvMsgDownload
        , recvMsgCancel = f $ hoistIdle <$> recvMsgCancel
        }

marloweTransferServerPeer
  :: forall m a
   . (Functor m)
  => MarloweTransferServer m a
  -> PeerTraced MarloweTransfer 'AsServer 'StIdle m a
marloweTransferServerPeer = EffectTraced . fmap peerIdle . runMarloweTransferServer
  where
    peerIdle :: ServerStIdle m a -> PeerTraced MarloweTransfer 'AsServer 'StIdle m a
    peerIdle ServerStIdle{..} = AwaitTraced (ClientAgency TokIdle) \case
      MsgStartImport -> Receive $ EffectTraced $ peerCanUpload <$> recvMsgStartImport
      MsgRequestExport hash ->
        Respond (ServerAgency TokExport) $
          recvMsgRequestExport hash <&> \case
            SendMsgStartExport next -> Response MsgStartExport $ peerCanDownload next
            SendMsgContractNotFound next -> Response MsgContractNotFound $ peerIdle next
      MsgDone -> Closed TokDone recvMsgDone

    peerCanUpload :: ServerStCanUpload m a -> PeerTraced MarloweTransfer 'AsServer 'StCanUpload m a
    peerCanUpload ServerStCanUpload{..} = AwaitTraced (ClientAgency TokCanUpload) \case
      MsgUpload bundle ->
        Respond (ServerAgency TokUpload) $
          recvMsgUpload bundle <&> \case
            SendMsgUploaded hashes idle -> Response (MsgUploaded hashes) $ peerCanUpload idle
            SendMsgUploadFailed err next -> Response (MsgUploadFailed err) $ peerIdle next
      MsgImported -> Receive $ EffectTraced $ peerIdle <$> recvMsgImported

    peerCanDownload :: ServerStCanDownload m a -> PeerTraced MarloweTransfer 'AsServer 'StCanDownload m a
    peerCanDownload ServerStCanDownload{..} = AwaitTraced (ClientAgency TokCanDownload) \case
      MsgDownload i ->
        Respond (ServerAgency TokDownload) $
          recvMsgDownload i <&> \case
            SendMsgDownloaded bundle next -> Response (MsgDownloaded bundle) $ peerCanDownload next
            SendMsgExported next -> Response MsgExported $ peerIdle next
      MsgCancel -> Receive $ EffectTraced $ peerIdle <$> recvMsgCancel
