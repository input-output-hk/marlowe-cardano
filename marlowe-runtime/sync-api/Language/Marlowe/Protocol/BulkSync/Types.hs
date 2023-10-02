{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.BulkSync.Types where

import Control.Monad (join)
import Data.Binary (Binary (..), getWord8, putWord8)
import qualified Data.List.NonEmpty as NE
import Data.String (IsString (..))
import Data.Word (Word8)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.History.Api (
  MarloweBlock,
 )
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Codec.Spec (
  MessageEq (..),
  MessageVariations (..),
  ShowProtocol,
  SomePeerHasAgency (..),
  Variations (..),
  varyAp,
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Peer.Trace (MessageAttributes (..), OTelProtocol (..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))

data MarloweBulkSync where
  StIdle :: MarloweBulkSync
  StIntersect :: MarloweBulkSync
  StNext :: MarloweBulkSync
  StPoll :: MarloweBulkSync
  StDone :: MarloweBulkSync

instance HasSignature MarloweBulkSync where
  signature _ = "MarloweBulkSync"

instance Protocol MarloweBulkSync where
  data Message MarloweBulkSync st st' where
    MsgRequestNext :: Word8 -> Message MarloweBulkSync 'StIdle 'StNext
    MsgRollForward :: [MarloweBlock] -> BlockHeader -> Message MarloweBulkSync 'StNext 'StIdle
    MsgRollBackward :: ChainPoint -> ChainPoint -> Message MarloweBulkSync 'StNext 'StIdle
    MsgWait :: Message MarloweBulkSync 'StNext 'StPoll
    MsgPoll :: Message MarloweBulkSync 'StPoll 'StNext
    MsgCancel :: Message MarloweBulkSync 'StPoll 'StIdle
    MsgIntersect :: [BlockHeader] -> Message MarloweBulkSync 'StIdle 'StIntersect
    MsgIntersectFound :: BlockHeader -> BlockHeader -> Message MarloweBulkSync 'StIntersect 'StIdle
    MsgIntersectNotFound :: ChainPoint -> Message MarloweBulkSync 'StIntersect 'StIdle
    MsgDone :: Message MarloweBulkSync 'StIdle 'StDone

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokPoll :: ClientHasAgency 'StPoll

  data ServerHasAgency st where
    TokIntersect :: ServerHasAgency 'StIntersect
    TokNext :: ServerHasAgency 'StNext

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokPoll = \case {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

instance BinaryMessage MarloweBulkSync where
  putMessage = \case
    ClientAgency TokIdle -> \case
      MsgRequestNext batchSize -> do
        putWord8 0x01
        put batchSize
      MsgIntersect blocks -> putWord8 0x02 *> put blocks
      MsgDone -> putWord8 0x03
    ServerAgency TokNext -> \case
      MsgRollForward block header -> do
        putWord8 0x04
        put block
        put header
      MsgRollBackward block tip -> do
        putWord8 0x05
        put block
        put tip
      MsgWait -> putWord8 0x06
    ClientAgency TokPoll -> \case
      MsgPoll -> putWord8 0x07
      MsgCancel -> putWord8 0x08
    ServerAgency TokIntersect -> \case
      MsgIntersectFound block tip -> do
        putWord8 0x09
        put block
        put tip
      MsgIntersectNotFound tip -> do
        putWord8 0x0a
        put tip

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokIdle -> SomeMessage . MsgRequestNext <$> get
        _ -> fail "Invalid protocol state for MsgRequestNext"
      0x02 -> case tok of
        ClientAgency TokIdle -> SomeMessage . MsgIntersect <$> get
        _ -> fail "Invalid protocol state for MsgRollForward"
      0x03 -> case tok of
        ClientAgency TokIdle -> pure $ SomeMessage MsgDone
        _ -> fail "Invalid protocol state for MsgDone"
      0x04 -> case tok of
        ServerAgency TokNext -> fmap SomeMessage . MsgRollForward <$> get <*> get
        _ -> fail "Invalid protocol state for MsgRollForward"
      0x05 -> case tok of
        ServerAgency TokNext -> fmap SomeMessage . MsgRollBackward <$> get <*> get
        _ -> fail "Invalid protocol state for MsgRollBackward"
      0x06 -> case tok of
        ServerAgency TokNext -> pure $ SomeMessage MsgWait
        _ -> fail "Invalid protocol state for MsgWait"
      0x07 -> case tok of
        ClientAgency TokPoll -> pure $ SomeMessage MsgPoll
        _ -> fail "Invalid protocol state for MsgPoll"
      0x08 -> case tok of
        ClientAgency TokPoll -> pure $ SomeMessage MsgCancel
        _ -> fail "Invalid protocol state for MsgCancel"
      0x09 -> case tok of
        ServerAgency TokIntersect -> fmap SomeMessage . MsgIntersectFound <$> get <*> get
        _ -> fail "Invalid protocol state for MsgIntersectFound"
      0x0a -> case tok of
        ServerAgency TokIntersect -> SomeMessage . MsgIntersectNotFound <$> get
        _ -> fail "Invalid protocol state for MsgIntersectNotFound"
      _ -> fail $ "Invalid message tag " <> show tag

deriving instance Show (Message MarloweBulkSync st st')
deriving instance Eq (Message MarloweBulkSync st st')
deriving instance Show (ClientHasAgency (st :: MarloweBulkSync))
deriving instance Eq (ClientHasAgency (st :: MarloweBulkSync))
deriving instance Show (ServerHasAgency (st :: MarloweBulkSync))
deriving instance Eq (ServerHasAgency (st :: MarloweBulkSync))
deriving instance Show (NobodyHasAgency (st :: MarloweBulkSync))
deriving instance Eq (NobodyHasAgency (st :: MarloweBulkSync))

instance MessageVariations MarloweBulkSync where
  agencyVariations =
    NE.fromList
      [ SomePeerHasAgency $ ClientAgency TokIdle
      , SomePeerHasAgency $ ClientAgency TokPoll
      , SomePeerHasAgency $ ServerAgency TokNext
      , SomePeerHasAgency $ ServerAgency TokIntersect
      ]
  messageVariations = \case
    ClientAgency TokIdle ->
      join $
        NE.fromList
          [ pure $ SomeMessage MsgDone
          , SomeMessage . MsgRequestNext <$> variations
          ]
    ClientAgency TokPoll -> NE.fromList [SomeMessage MsgPoll, SomeMessage MsgCancel]
    ServerAgency TokNext ->
      join $
        NE.fromList
          [ fmap SomeMessage . MsgRollForward <$> variations `varyAp` variations
          , fmap SomeMessage . MsgRollBackward <$> variations `varyAp` variations
          , pure $ SomeMessage MsgWait
          ]
    ServerAgency TokIntersect ->
      join $
        NE.fromList
          [ fmap SomeMessage . MsgIntersectFound <$> variations `varyAp` variations
          , SomeMessage . MsgIntersectNotFound <$> variations
          ]

instance MessageEq MarloweBulkSync where
  messageEq (AnyMessageAndAgency _ msg1) (AnyMessageAndAgency _ msg2) = case msg1 of
    MsgIntersect{} -> case msg2 of
      MsgIntersect{} -> msg1 == msg2
      _ -> False
    MsgDone -> case msg2 of
      MsgDone -> True
      _ -> False
    MsgRequestNext{} -> case msg2 of
      MsgRequestNext{} -> msg1 == msg2
      _ -> False
    MsgRollForward{} -> case msg2 of
      MsgRollForward{} -> msg1 == msg2
      _ -> False
    MsgRollBackward{} -> case msg2 of
      MsgRollBackward{} -> msg1 == msg2
      _ -> False
    MsgWait -> case msg2 of
      MsgWait -> True
      _ -> False
    MsgPoll -> case msg2 of
      MsgPoll -> True
      _ -> False
    MsgCancel -> case msg2 of
      MsgCancel -> True
      _ -> False
    MsgIntersectFound{} -> case msg2 of
      MsgIntersectFound{} -> msg1 == msg2
      _ -> False
    MsgIntersectNotFound{} -> case msg2 of
      MsgIntersectNotFound{} -> msg1 == msg2
      _ -> False

instance ShowProtocol MarloweBulkSync

instance OTelProtocol MarloweBulkSync where
  protocolName _ = "marlowe_bulk_sync"
  messageAttributes = \case
    ClientAgency tok -> case tok of
      TokIdle -> \case
        MsgRequestNext batchSize ->
          MessageAttributes
            { messageType = "request_next"
            , messageParameters = fromString <$> [show batchSize]
            }
        MsgIntersect blocks ->
          MessageAttributes
            { messageType = "intersect"
            , messageParameters = fromString . show <$> blocks
            }
        MsgDone ->
          MessageAttributes
            { messageType = "done"
            , messageParameters = []
            }
      TokPoll -> \case
        MsgPoll ->
          MessageAttributes
            { messageType = "poll"
            , messageParameters = []
            }
        MsgCancel ->
          MessageAttributes
            { messageType = "cancel"
            , messageParameters = []
            }
    ServerAgency tok -> case tok of
      TokNext -> \case
        MsgRollForward block headers ->
          MessageAttributes
            { messageType = "request_next/new_headers"
            , messageParameters = fromString <$> [show block, show headers]
            }
        MsgRollBackward block tip ->
          MessageAttributes
            { messageType = "request_next/roll_backward"
            , messageParameters = fromString <$> [show block, show tip]
            }
        MsgWait ->
          MessageAttributes
            { messageType = "request_next/wait"
            , messageParameters = []
            }
      TokIntersect -> \case
        MsgIntersectFound block tip ->
          MessageAttributes
            { messageType = "intersect/found"
            , messageParameters = fromString <$> [show block, show tip]
            }
        MsgIntersectNotFound tip ->
          MessageAttributes
            { messageType = "intersect/not_found"
            , messageParameters = fromString <$> [show tip]
            }
