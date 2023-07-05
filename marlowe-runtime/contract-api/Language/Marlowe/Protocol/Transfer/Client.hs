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
import Numeric.Natural (Natural)

newtype MarloweTransferClient m a = MarloweTransferClient
  { runMarloweTransferClient :: m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStIdle m a where
  SendMsgStartImport :: ClientStCanUpload m a -> ClientStIdle m a
  SendMsgStartExport :: DatumHash -> ClientStCanDownload m a -> ClientStIdle m a
  SendMsgDone :: a -> ClientStIdle m a

deriving instance (Functor m) => Functor (ClientStIdle m)

data ClientStCanUpload m a where
  SendMsgUpload :: ObjectBundle -> ClientStUpload m a -> ClientStCanUpload m a
  SendMsgImported :: ClientStIdle m a -> ClientStCanUpload m a

data ClientStDownload m a = ClientStDownload
  { recvMsgDownloaded :: ObjectBundle -> m (ClientStCanDownload m a)
  , recvMsgExported :: m (ClientStIdle m a)
  }
  deriving (Functor)

deriving instance (Functor m) => Functor (ClientStCanUpload m)

data ClientStUpload m a = ClientStUpload
  { recvMsgUploaded :: Map Label DatumHash -> m (ClientStCanUpload m a)
  , recvMsgUploadFailed :: ImportError -> m (ClientStIdle m a)
  }
  deriving (Functor)

data ClientStCanDownload m a where
  SendMsgDownload :: Natural -> ClientStDownload m a -> ClientStCanDownload m a
  SendMsgCancel :: ClientStIdle m a -> ClientStCanDownload m a

deriving instance (Functor m) => Functor (ClientStCanDownload m)

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
      SendMsgStartImport next -> SendMsgStartImport $ hoistCanUpload next
      SendMsgStartExport hash next -> SendMsgStartExport hash $ hoistCanDownload next
      SendMsgDone a -> SendMsgDone a

    hoistCanUpload :: ClientStCanUpload m a -> ClientStCanUpload n a
    hoistCanUpload = \case
      SendMsgUpload bundle next -> SendMsgUpload bundle $ hoistUpload next
      SendMsgImported next -> SendMsgImported $ hoistIdle next

    hoistUpload :: ClientStUpload m a -> ClientStUpload n a
    hoistUpload ClientStUpload{..} =
      ClientStUpload
        { recvMsgUploaded = f . fmap hoistCanUpload . recvMsgUploaded
        , recvMsgUploadFailed = f . fmap hoistIdle . recvMsgUploadFailed
        }

    hoistDownload :: ClientStDownload m a -> ClientStDownload n a
    hoistDownload ClientStDownload{..} =
      ClientStDownload
        { recvMsgDownloaded = f . fmap hoistCanDownload . recvMsgDownloaded
        , recvMsgExported = f $ hoistIdle <$> recvMsgExported
        }

    hoistCanDownload :: ClientStCanDownload m a -> ClientStCanDownload n a
    hoistCanDownload = \case
      SendMsgDownload i next -> SendMsgDownload i $ hoistDownload next
      SendMsgCancel next -> SendMsgCancel $ hoistIdle next

marloweTransferClientPeer
  :: forall m a
   . (Functor m)
  => MarloweTransferClient m a
  -> PeerTraced MarloweTransfer 'AsClient 'StIdle m a
marloweTransferClientPeer = EffectTraced . fmap peerIdle . runMarloweTransferClient
  where
    peerIdle :: ClientStIdle m a -> PeerTraced MarloweTransfer 'AsClient 'StIdle m a
    peerIdle = \case
      SendMsgStartImport next ->
        YieldTraced (ClientAgency TokIdle) MsgStartImport $ Cast $ peerCanUpload next
      SendMsgStartExport hash next ->
        YieldTraced (ClientAgency TokIdle) (MsgStartExport hash) $ Cast $ peerCanDownload next
      SendMsgDone a -> YieldTraced (ClientAgency TokIdle) MsgDone $ Close TokDone a

    peerCanUpload :: ClientStCanUpload m a -> PeerTraced MarloweTransfer 'AsClient 'StCanUpload m a
    peerCanUpload = \case
      SendMsgUpload bundle ClientStUpload{..} ->
        YieldTraced (ClientAgency TokCanUpload) (MsgUpload bundle) $
          Call (ServerAgency TokUpload) \case
            MsgUploaded hashes -> EffectTraced $ peerCanUpload <$> recvMsgUploaded hashes
            MsgUploadFailed err -> EffectTraced $ peerIdle <$> recvMsgUploadFailed err
      SendMsgImported next ->
        YieldTraced (ClientAgency TokCanUpload) MsgImported $ Cast $ peerIdle next

    peerCanDownload :: ClientStCanDownload m a -> PeerTraced MarloweTransfer 'AsClient 'StCanDownload m a
    peerCanDownload = \case
      SendMsgDownload i ClientStDownload{..} ->
        YieldTraced (ClientAgency TokCanDownload) (MsgDownload i) $
          Call (ServerAgency TokDownload) \case
            MsgDownloaded bundle -> EffectTraced $ peerCanDownload <$> recvMsgDownloaded bundle
            MsgExported -> EffectTraced $ peerIdle <$> recvMsgExported
      SendMsgCancel next ->
        YieldTraced (ClientAgency TokCanDownload) MsgCancel $ Cast $ peerIdle next

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
      SendMsgStartImport next -> flip serveCanUpload next =<< recvMsgStartImport
      SendMsgStartExport hash next -> flip serveCanDownload next =<< recvMsgStartExport hash
      SendMsgDone b -> (,b) <$> recvMsgDone

    serveCanUpload :: ServerStCanUpload m a -> ClientStCanUpload m b -> m (a, b)
    serveCanUpload ServerStCanUpload{..} = \case
      SendMsgUpload bundle next -> serveUpload next =<< recvMsgUpload bundle
      SendMsgImported next -> flip serveIdle next =<< recvMsgImported

    serveUpload :: ClientStUpload m b -> ServerStUpload m a -> m (a, b)
    serveUpload ClientStUpload{..} = \case
      SendMsgUploaded hash next -> serveCanUpload next =<< recvMsgUploaded hash
      SendMsgUploadFailed err next -> serveIdle next =<< recvMsgUploadFailed err

    serveDownload :: ClientStDownload m b -> ServerStDownload m a -> m (a, b)
    serveDownload ClientStDownload{..} = \case
      SendMsgDownloaded bundle next -> serveCanDownload next =<< recvMsgDownloaded bundle
      SendMsgExported next -> serveIdle next =<< recvMsgExported

    serveCanDownload :: ServerStCanDownload m a -> ClientStCanDownload m b -> m (a, b)
    serveCanDownload ServerStCanDownload{..} = \case
      SendMsgDownload i next -> serveDownload next =<< recvMsgDownload i
      SendMsgCancel next -> flip serveIdle next =<< recvMsgCancel
