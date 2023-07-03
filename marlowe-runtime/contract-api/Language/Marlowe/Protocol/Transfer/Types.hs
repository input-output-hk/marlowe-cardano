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

data TransferError
  = MerkleizedContractsNotAllowed
  | LinkError LinkError
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (FromJSON, ToJSON, Binary, Variations)

data MarloweTransfer where
  StIdle :: MarloweTransfer
  StTransfer :: MarloweTransfer
  StDone :: MarloweTransfer

instance HasSignature MarloweTransfer where
  signature _ = "MarloweTransfer"

instance Protocol MarloweTransfer where
  data Message MarloweTransfer st st' where
    MsgTransfer :: ObjectBundle -> Message MarloweTransfer 'StIdle 'StTransfer
    MsgTransferred :: Map Label DatumHash -> Message MarloweTransfer 'StTransfer 'StIdle
    MsgTransferFailed :: TransferError -> Message MarloweTransfer 'StTransfer 'StDone
    MsgDone :: Message MarloweTransfer 'StIdle 'StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle

  data ServerHasAgency st where
    TokTransfer :: ServerHasAgency 'StTransfer

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case {}
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
      MsgTransfer bundle -> do
        putWord8 0
        put bundle
      MsgDone -> putWord8 1
    ServerAgency TokTransfer -> \case
      MsgTransferred hashes -> do
        putWord8 0
        put hashes
      MsgTransferFailed err -> do
        putWord8 1
        put err
  getMessage tok = do
    tag <- getWord8
    case tok of
      ClientAgency TokIdle -> case tag of
        0 -> SomeMessage . MsgTransfer <$> get
        1 -> pure $ SomeMessage MsgDone
        _ -> fail $ "invalid tag byte " <> show tag
      ServerAgency TokTransfer -> case tag of
        0 -> SomeMessage . MsgTransferred <$> get
        1 -> SomeMessage . MsgTransferFailed <$> get
        _ -> fail $ "invalid tag byte " <> show tag

instance OTelProtocol MarloweTransfer where
  protocolName _ = "marlowe_transfer"
  messageAttributes = \case
    ClientAgency TokIdle -> \case
      MsgTransfer bundle ->
        MessageAttributes
          { messageType = "transfer"
          , messageParameters = [jsonToPrimitiveAttribute $ toJSON bundle]
          }
      MsgDone ->
        MessageAttributes
          { messageType = "done"
          , messageParameters = []
          }
    ServerAgency TokTransfer -> \case
      MsgTransferred hashes ->
        MessageAttributes
          { messageType = "transferred"
          , messageParameters = [fromString $ read $ show hashes]
          }
      MsgTransferFailed err ->
        MessageAttributes
          { messageType = "transfer_failed"
          , messageParameters = [jsonToPrimitiveAttribute $ toJSON err]
          }

instance MessageEq MarloweTransfer where
  messageEq (AnyMessageAndAgency _ msg) (AnyMessageAndAgency _ msg') = case msg of
    MsgTransfer bundle -> case msg' of
      MsgTransfer bundle' -> bundle == bundle'
      _ -> False
    MsgTransferred hashes -> case msg' of
      MsgTransferred hashes' -> hashes == hashes'
      _ -> False
    MsgTransferFailed err -> case msg' of
      MsgTransferFailed err' -> err == err'
      _ -> False
    MsgDone -> case msg' of
      MsgDone -> True
      _ -> False

instance MessageVariations MarloweTransfer where
  messageVariations = \case
    ClientAgency TokIdle ->
      join $
        NE.fromList
          [ SomeMessage . MsgTransfer <$> variations
          , pure $ SomeMessage MsgDone
          ]
    ServerAgency TokTransfer ->
      join $
        NE.fromList
          [ SomeMessage . MsgTransferred <$> variations
          , SomeMessage . MsgTransferFailed <$> variations
          ]
  agencyVariations =
    NE.fromList
      [ SomePeerHasAgency $ ClientAgency TokIdle
      , SomePeerHasAgency $ ServerAgency TokTransfer
      ]

instance ShowProtocol MarloweTransfer
