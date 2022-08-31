{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.Sync.Codec where

import Data.Binary (get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Language.Marlowe.Protocol.Sync.Types
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), SomeMarloweVersion (..))
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.TypedProtocol.Codec (Codec, PeerHasAgency (..), SomeMessage (SomeMessage))

codecMarloweSync :: Applicative m => Codec MarloweSync DeserializeError m LBS.ByteString
codecMarloweSync = binaryCodec putMessage getMessage
  where
  putMessage :: PutMessage MarloweSync
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgFollowContract contractId -> do
        putWord8 0x01
        put contractId
      MsgIntersect contractId version blockHeaders -> do
        putWord8 0x02
        put contractId
        put $ SomeMarloweVersion version
        put blockHeaders

    ServerAgency TokFollow -> \case
      MsgContractNotFound -> putWord8 0x03
      MsgContractFound blockHeader version createStep -> do
        putWord8 0x04
        put blockHeader
        put $ SomeMarloweVersion version
        case version of
          MarloweV1 -> put createStep

    ClientAgency (TokIdle _) -> \case
      MsgRequestNext -> putWord8 0x05
      MsgDone        -> putWord8 0x06

    ServerAgency (TokNext version) -> \case
      MsgRollForward blockHeader contractSteps -> do
        putWord8 0x07
        put blockHeader
        case version of
          MarloweV1 -> put contractSteps
      MsgRollBackward blockHeader -> do
        putWord8 0x08
        put blockHeader
      MsgRollBackCreation -> putWord8 0x0e

      MsgWait -> putWord8 0x09

    ClientAgency (TokWait _) -> \case
      MsgPoll   -> putWord8 0x0a
      MsgCancel -> putWord8 0x0b

    ServerAgency (TokIntersect _) -> \case
      MsgIntersectFound blockHeader -> do
        putWord8 0x0c
        put blockHeader
      MsgIntersectNotFound -> putWord8 0x0d

  getMessage :: GetMessage MarloweSync
  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> SomeMessage . MsgFollowContract <$> get
        _                    -> fail "Invalid protocol state for MsgFollowContract"

      0x02 -> case tok of
        ClientAgency TokInit -> do
          contractId <- get
          SomeMarloweVersion version <- get
          SomeMessage . MsgIntersect contractId version <$> get
        _                        -> fail "Invalid protocol state for MsgIntersect"

      0x03 -> case tok of
        ServerAgency TokFollow -> pure $ SomeMessage MsgContractNotFound
        _                      -> fail "Invalid protocol state for MsgContractNotFound"

      0x04 -> case tok of
        ServerAgency TokFollow -> do
          blockHeader <- get @BlockHeader
          SomeMarloweVersion version <- get
          case version of
            MarloweV1 -> SomeMessage . MsgContractFound blockHeader version <$> get
        _ -> fail "Invalid protocol state for MsgContractFound"

      0x05 -> case tok of
        ClientAgency (TokIdle _) -> pure $ SomeMessage MsgRequestNext
        _                        -> fail "Invalid protocol state for MsgRequestNext"

      0x06 -> case tok of
        ClientAgency (TokIdle _) -> pure $ SomeMessage MsgDone
        _                        -> fail "Invalid protocol state for MsgDone"

      0x07 -> case tok of
        ServerAgency (TokNext version) -> do
          blockHeader <- get @BlockHeader
          case version of
            MarloweV1 -> SomeMessage . MsgRollForward blockHeader <$> get
        _ -> fail "Invalid protocol state for MsgRollForward"

      0x08 -> case tok of
        ServerAgency (TokNext _) -> SomeMessage . MsgRollBackward <$> get
        _                        -> fail "Invalid protocol state for MsgRollBackward"

      0x09 -> case tok of
        ServerAgency (TokNext _) -> pure $ SomeMessage MsgWait
        _                        -> fail "Invalid protocol state for MsgWait"

      0x0a -> case tok of
        ClientAgency (TokWait _) -> pure $ SomeMessage MsgPoll
        _                        -> fail "Invalid protocol state for MsgPoll"

      0x0b -> case tok of
        ClientAgency (TokWait _) -> pure $ SomeMessage MsgCancel
        _                        -> fail "Invalid protocol state for MsgCancel"

      0x0c -> case tok of
        ServerAgency (TokIntersect _) -> SomeMessage . MsgIntersectFound <$> get
        _                             -> fail "Invalid protocol state for MsgIntersectFound"

      0x0d -> case tok of
        ServerAgency (TokIntersect _) -> pure $ SomeMessage MsgIntersectNotFound
        _                             -> fail "Invalid protocol state for MsgIntersectNotFound"

      0x0e -> case tok of
        ServerAgency (TokNext _) -> pure $ SomeMessage MsgRollBackCreation
        _                        -> fail "Invalid protocol state for MsgRollBackCreation"

      _ -> fail $ "Invalid message tag " <> show tag
