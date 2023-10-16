{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Transfer.Types where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON (toJSON))
import Data.Binary (Binary (..), getWord8, putWord8)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.String (fromString)
import GHC.Generics (Generic)
import Language.Marlowe.Object.Link (LinkError)
import Language.Marlowe.Object.Types
import Language.Marlowe.Protocol.Load.Types (jsonToPrimitiveAttribute)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Codec.Spec (
  MessageEq (..),
  MessageVariations (..),
  ShowProtocol (..),
  SomePeerHasAgency (..),
  Variations (..),
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Peer.Trace (MessageAttributes (..), OTelProtocol (..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import Numeric.Natural (Natural)
import OpenTelemetry.Attributes (PrimitiveAttribute (IntAttribute))

data ImportError
  = ContinuationNotInStore ContractHash
  | LinkError LinkError
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Binary, Variations)

data MarloweTransfer where
  StIdle :: MarloweTransfer
  StExport :: MarloweTransfer
  StCanUpload :: MarloweTransfer
  StUpload :: MarloweTransfer
  StCanDownload :: MarloweTransfer
  StDownload :: MarloweTransfer
  StDone :: MarloweTransfer

instance HasSignature MarloweTransfer where
  signature _ = "MarloweTransfer"

instance Protocol MarloweTransfer where
  data Message MarloweTransfer st st' where
    MsgStartImport :: Message MarloweTransfer 'StIdle 'StCanUpload
    MsgRequestExport :: DatumHash -> Message MarloweTransfer 'StIdle 'StExport
    MsgStartExport :: Message MarloweTransfer 'StExport 'StCanDownload
    MsgContractNotFound :: Message MarloweTransfer 'StExport 'StIdle
    MsgDone :: Message MarloweTransfer 'StIdle 'StDone
    MsgUpload :: ObjectBundle -> Message MarloweTransfer 'StCanUpload 'StUpload
    MsgUploaded :: Map Label DatumHash -> Message MarloweTransfer 'StUpload 'StCanUpload
    MsgUploadFailed :: ImportError -> Message MarloweTransfer 'StUpload 'StIdle
    MsgImported :: Message MarloweTransfer 'StCanUpload 'StIdle
    MsgDownloaded :: ObjectBundle -> Message MarloweTransfer 'StDownload 'StCanDownload
    MsgDownload :: Natural -> Message MarloweTransfer 'StCanDownload 'StDownload
    MsgCancel :: Message MarloweTransfer 'StCanDownload 'StIdle
    MsgExported :: Message MarloweTransfer 'StDownload 'StIdle

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokCanUpload :: ClientHasAgency 'StCanUpload
    TokCanDownload :: ClientHasAgency 'StCanDownload

  data ServerHasAgency st where
    TokExport :: ServerHasAgency 'StExport
    TokUpload :: ServerHasAgency 'StUpload
    TokDownload :: ServerHasAgency 'StDownload

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokCanUpload = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokCanDownload = \case {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

deriving instance Show (Message MarloweTransfer st st')
deriving instance Eq (Message MarloweTransfer st st')
deriving instance Ord (Message MarloweTransfer st st')

deriving instance Show (ClientHasAgency (st :: MarloweTransfer))
deriving instance Eq (ClientHasAgency (st :: MarloweTransfer))
deriving instance Ord (ClientHasAgency (st :: MarloweTransfer))

deriving instance Show (ServerHasAgency (st :: MarloweTransfer))
deriving instance Eq (ServerHasAgency (st :: MarloweTransfer))
deriving instance Ord (ServerHasAgency (st :: MarloweTransfer))

deriving instance Show (NobodyHasAgency (st :: MarloweTransfer))
deriving instance Eq (NobodyHasAgency (st :: MarloweTransfer))
deriving instance Ord (NobodyHasAgency (st :: MarloweTransfer))

instance BinaryMessage MarloweTransfer where
  putMessage = \case
    ClientAgency TokIdle -> \case
      MsgStartImport -> putWord8 0
      MsgRequestExport hash -> do
        putWord8 1
        put hash
      MsgDone -> putWord8 2
    ClientAgency TokCanUpload -> \case
      MsgUpload bundle -> do
        putWord8 0
        put bundle
      MsgImported -> putWord8 1
    ClientAgency TokCanDownload -> \case
      MsgDownload i -> do
        putWord8 0
        put i
      MsgCancel -> putWord8 1
    ServerAgency TokExport -> \case
      MsgStartExport -> putWord8 0
      MsgContractNotFound -> putWord8 1
    ServerAgency TokUpload -> \case
      MsgUploaded hashes -> do
        putWord8 0
        put hashes
      MsgUploadFailed err -> do
        putWord8 1
        put err
    ServerAgency TokDownload -> \case
      MsgDownloaded bundle -> do
        putWord8 0
        put bundle
      MsgExported -> putWord8 1
  getMessage tok = do
    tag <- getWord8
    case tok of
      ClientAgency TokIdle -> case tag of
        0 -> pure $ SomeMessage MsgStartImport
        1 -> SomeMessage . MsgRequestExport <$> get
        2 -> pure $ SomeMessage MsgDone
        _ -> fail $ "invalid tag byte " <> show tag
      ClientAgency TokCanUpload -> case tag of
        0 -> SomeMessage . MsgUpload <$> get
        1 -> pure $ SomeMessage MsgImported
        _ -> fail $ "invalid tag byte " <> show tag
      ClientAgency TokCanDownload -> case tag of
        0 -> SomeMessage . MsgDownload <$> get
        1 -> pure $ SomeMessage MsgCancel
        _ -> fail $ "invalid tag byte " <> show tag
      ServerAgency TokExport -> case tag of
        0 -> pure $ SomeMessage MsgStartExport
        1 -> pure $ SomeMessage MsgContractNotFound
        _ -> fail $ "invalid tag byte " <> show tag
      ServerAgency TokUpload -> case tag of
        0 -> SomeMessage . MsgUploaded <$> get
        1 -> SomeMessage . MsgUploadFailed <$> get
        _ -> fail $ "invalid tag byte " <> show tag
      ServerAgency TokDownload -> case tag of
        0 -> SomeMessage . MsgDownloaded <$> get
        1 -> pure $ SomeMessage MsgExported
        _ -> fail $ "invalid tag byte " <> show tag

instance OTelProtocol MarloweTransfer where
  protocolName _ = "marlowe_transfer"
  messageAttributes = \case
    ClientAgency TokIdle -> \case
      MsgStartImport ->
        MessageAttributes
          { messageType = "start_import"
          , messageParameters = []
          }
      MsgRequestExport hash ->
        MessageAttributes
          { messageType = "request_export"
          , messageParameters = [fromString $ read $ show hash]
          }
      MsgDone ->
        MessageAttributes
          { messageType = "done"
          , messageParameters = []
          }
    ClientAgency TokCanUpload -> \case
      MsgUpload bundle ->
        MessageAttributes
          { messageType = "upload"
          , messageParameters = [jsonToPrimitiveAttribute $ toJSON bundle]
          }
      MsgImported ->
        MessageAttributes
          { messageType = "imported"
          , messageParameters = []
          }
    ClientAgency TokCanDownload -> \case
      MsgDownload i ->
        MessageAttributes
          { messageType = "download"
          , messageParameters = [IntAttribute $ fromIntegral i]
          }
      MsgCancel ->
        MessageAttributes
          { messageType = "cancel"
          , messageParameters = []
          }
    ServerAgency TokUpload -> \case
      MsgUploaded hashes ->
        MessageAttributes
          { messageType = "uploaded"
          , messageParameters = [jsonToPrimitiveAttribute $ toJSON hashes]
          }
      MsgUploadFailed err ->
        MessageAttributes
          { messageType = "upload_failed"
          , messageParameters = [jsonToPrimitiveAttribute $ toJSON err]
          }
    ServerAgency TokDownload -> \case
      MsgDownloaded bundle ->
        MessageAttributes
          { messageType = "downloaded"
          , messageParameters = [jsonToPrimitiveAttribute $ toJSON bundle]
          }
      MsgExported ->
        MessageAttributes
          { messageType = "exported"
          , messageParameters = []
          }
    ServerAgency TokExport -> \case
      MsgStartExport ->
        MessageAttributes
          { messageType = "start_export"
          , messageParameters = []
          }
      MsgContractNotFound ->
        MessageAttributes
          { messageType = "contract_not_found"
          , messageParameters = []
          }

instance MessageEq MarloweTransfer where
  messageEq (AnyMessageAndAgency _ msg) (AnyMessageAndAgency _ msg') = case msg of
    MsgStartImport -> case msg' of
      MsgStartImport -> True
      _ -> False
    MsgRequestExport hash -> case msg' of
      MsgRequestExport hash' -> hash == hash'
      _ -> False
    MsgDone -> case msg' of
      MsgDone -> True
      _ -> False
    MsgStartExport -> case msg' of
      MsgStartExport -> True
      _ -> False
    MsgContractNotFound -> case msg' of
      MsgContractNotFound -> True
      _ -> False
    MsgUpload bundle -> case msg' of
      MsgUpload bundle' -> bundle == bundle'
      _ -> False
    MsgUploaded hashes -> case msg' of
      MsgUploaded hashes' -> hashes == hashes'
      _ -> False
    MsgUploadFailed err -> case msg' of
      MsgUploadFailed err' -> err == err'
      _ -> False
    MsgImported -> case msg' of
      MsgImported -> True
      _ -> False
    MsgDownloaded bundle -> case msg' of
      MsgDownloaded bundle' -> bundle == bundle'
      _ -> False
    MsgDownload i -> case msg' of
      MsgDownload i' -> i == i'
      _ -> False
    MsgCancel -> case msg' of
      MsgCancel -> True
      _ -> False
    MsgExported -> case msg' of
      MsgExported -> True
      _ -> False

instance MessageVariations MarloweTransfer where
  messageVariations = \case
    ClientAgency TokIdle ->
      join $
        NE.fromList
          [ pure $ SomeMessage MsgStartImport
          , SomeMessage . MsgRequestExport <$> variations
          , pure $ SomeMessage MsgDone
          ]
    ClientAgency TokCanUpload ->
      join $
        NE.fromList
          [ SomeMessage . MsgUpload <$> variations
          , pure $ SomeMessage MsgImported
          ]
    ClientAgency TokCanDownload ->
      join $
        NE.fromList
          [ SomeMessage . MsgDownload <$> variations
          , pure $ SomeMessage MsgCancel
          ]
    ServerAgency TokExport ->
      join $
        NE.fromList
          [ pure $ SomeMessage MsgStartExport
          , pure $ SomeMessage MsgContractNotFound
          ]
    ServerAgency TokUpload ->
      join $
        NE.fromList
          [ SomeMessage . MsgUploaded <$> variations
          , SomeMessage . MsgUploadFailed <$> variations
          ]
    ServerAgency TokDownload ->
      join $
        NE.fromList
          [ SomeMessage . MsgDownloaded <$> variations
          , pure $ SomeMessage MsgExported
          ]
  agencyVariations =
    NE.fromList
      [ SomePeerHasAgency $ ClientAgency TokIdle
      , SomePeerHasAgency $ ServerAgency TokExport
      , SomePeerHasAgency $ ClientAgency TokCanUpload
      , SomePeerHasAgency $ ServerAgency TokUpload
      , SomePeerHasAgency $ ClientAgency TokCanDownload
      , SomePeerHasAgency $ ServerAgency TokDownload
      ]

instance ShowProtocol MarloweTransfer
