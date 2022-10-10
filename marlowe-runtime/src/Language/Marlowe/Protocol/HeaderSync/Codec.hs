{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.HeaderSync.Codec
  where

import Data.Binary (get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Language.Marlowe.Protocol.HeaderSync.Types
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.TypedProtocol.Codec (Codec, PeerHasAgency(..), SomeMessage(SomeMessage))

codecMarloweHeaderSync :: Applicative m => Codec MarloweHeaderSync DeserializeError m LBS.ByteString
codecMarloweHeaderSync = binaryCodec putMessage getMessage
  where
  putMessage :: PutMessage MarloweHeaderSync
  putMessage = \case
    ClientAgency TokIdle -> \case
      MsgRequestNext -> putWord8 0x01
      MsgIntersect blocks -> putWord8 0x02 *> put blocks
      MsgDone -> putWord8 0x03

    ServerAgency TokNext -> \case
      MsgNewHeaders block headers -> do
        putWord8 0x04
        put block
        put headers
      MsgRollBackward block -> do
        putWord8 0x05
        put block

      MsgWait -> putWord8 0x06

    ClientAgency TokWait -> \case
      MsgPoll -> putWord8 0x07
      MsgCancel -> putWord8 0x08

    ServerAgency TokIntersect -> \case
      MsgIntersectFound block -> do
        putWord8 0x09
        put block
      MsgIntersectNotFound -> putWord8 0x0a

  getMessage :: GetMessage MarloweHeaderSync
  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokIdle -> pure $ SomeMessage MsgRequestNext
        _                        -> fail "Invalid protocol state for MsgRequestNext"

      0x02 -> case tok of
        ClientAgency TokIdle -> SomeMessage .MsgIntersect <$> get
        _ -> fail "Invalid protocol state for MsgNewHeaders"

      0x03 -> case tok of
        ClientAgency TokIdle -> pure $ SomeMessage MsgDone
        _                        -> fail "Invalid protocol state for MsgDone"

      0x04 -> case tok of
        ServerAgency TokNext -> do
          block <- get
          SomeMessage . MsgNewHeaders block <$> get
        _ -> fail "Invalid protocol state for MsgNewHeaders"

      0x05 -> case tok of
        ServerAgency TokNext -> SomeMessage . MsgRollBackward <$> get
        _                        -> fail "Invalid protocol state for MsgRollBackward"

      0x06 -> case tok of
        ServerAgency TokNext -> pure $ SomeMessage MsgWait
        _                        -> fail "Invalid protocol state for MsgWait"

      0x07 -> case tok of
        ClientAgency TokWait -> pure $ SomeMessage MsgPoll
        _                        -> fail "Invalid protocol state for MsgPoll"

      0x08 -> case tok of
        ClientAgency TokWait -> pure $ SomeMessage MsgCancel
        _                        -> fail "Invalid protocol state for MsgCancel"

      0x09 -> case tok of
        ServerAgency TokIntersect -> SomeMessage . MsgIntersectFound <$> get
        _                             -> fail "Invalid protocol state for MsgIntersectFound"

      0x0a -> case tok of
        ServerAgency TokIntersect -> pure $ SomeMessage MsgIntersectNotFound
        _                             -> fail "Invalid protocol state for MsgIntersectNotFound"

      _ -> fail $ "Invalid message tag " <> show tag
