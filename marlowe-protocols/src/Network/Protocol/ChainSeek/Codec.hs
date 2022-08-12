{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

module Network.Protocol.ChainSeek.Codec (DeserializeError, SomeQuery(..), codecChainSeek) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.ChainSeek.Types (ChainSeek, ClientHasAgency (..), Message (..), ServerHasAgency (..),
                                         TokNextKind (..))
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

data SomeQuery query = forall err result. SomeQuery (query err result)

codecChainSeek
  :: forall query point tip m
   . Applicative m
  => (SomeQuery query -> Put)
  -> Get (SomeQuery query)
  -> (forall err result. query err result -> result -> Put)
  -> (forall err result. query err result -> Get result)
  -> (forall err result. query err result -> err -> Put)
  -> (forall err result. query err result -> Get err)
  -> (point -> Put)
  -> Get point
  -> (tip -> Put)
  -> Get tip
  -> Codec (ChainSeek query point tip) DeserializeError m LBS.ByteString
codecChainSeek
  encodeQuery
  decodeQuery
  encodeResult
  decodeResult
  encodeError
  decodeError
  encodePoint
  decodePoint
  encodeTip
  decodeTip = binaryCodec putMsg getMsg
  where
    putMsg :: PutMessage (ChainSeek query point tip)
    putMsg (ClientAgency TokInit) msg = case msg of
      MsgRequestHandshake schemaVersion -> do
        putWord8 0x01
        put schemaVersion

    putMsg (ClientAgency TokIdle) msg = case msg of
      MsgQueryNext query -> do
        putWord8 0x04
        encodeQuery $ SomeQuery query
      MsgDone            -> putWord8 0x09

    putMsg (ServerAgency TokHandshake) msg = case msg of
      MsgConfirmHandshake                  -> putWord8 0x02
      MsgRejectHandshake supportedVersions -> do
       putWord8 0x03
       put supportedVersions

    putMsg (ServerAgency (TokNext query _)) (MsgRejectQuery err tip) = do
        putWord8 0x05
        -- FIXME how to do this without unsafeCoerce?
        encodeError (unsafeCoerce query) err
        encodeTip tip

    putMsg (ServerAgency (TokNext query _)) (MsgRollForward result pos tip) = do
        putWord8 0x06
        -- FIXME how to do this without unsafeCoerce?
        encodeResult (unsafeCoerce query) result
        encodePoint pos
        encodeTip tip

    putMsg (ServerAgency TokNext{}) (MsgRollBackward pos tip) = do
        putWord8 0x07
        encodePoint pos
        encodeTip tip

    putMsg (ServerAgency TokNext{}) MsgWait = putWord8 0x08

    putMsg (ServerAgency TokNext{}) MsgPing = putWord8 0x0a

    putMsg (ClientAgency TokPing) MsgPong = putWord8 0x0b

    getMsg :: GetMessage (ChainSeek query point tip)
    getMsg tok      = do
      tag <- getWord8
      case (tag, tok) of
        (0x01, ClientAgency TokInit) -> SomeMessage . MsgRequestHandshake <$> get

        (0x04, ClientAgency TokIdle) -> do
          SomeQuery query <- decodeQuery
          pure $ SomeMessage $ MsgQueryNext query

        (0x09, ClientAgency TokIdle) -> pure $ SomeMessage MsgDone

        (0x02, ServerAgency TokHandshake) -> pure $ SomeMessage MsgConfirmHandshake

        (0x03, ServerAgency TokHandshake) -> SomeMessage . MsgRejectHandshake <$> get

        (0x05, ServerAgency (TokNext query _)) -> do
          err <- decodeError (unsafeCoerce query)
          tip <- decodeTip
          -- FIXME how to do this without unsafeCoerce?
          pure $ unsafeCoerce $ SomeMessage $ MsgRejectQuery err tip

        (0x06, ServerAgency (TokNext query _)) -> do
          result <- decodeResult (unsafeCoerce query)
          point <- decodePoint
          tip <- decodeTip
          -- FIXME how to do this without unsafeCoerce?
          pure $ SomeMessage $ unsafeCoerce $ MsgRollForward result point tip

        (0x07, ServerAgency (TokNext _ _)) -> do
          point <- decodePoint
          tip <- decodeTip
          pure $ SomeMessage $ MsgRollBackward point tip

        (0x08, ServerAgency (TokNext _ TokCanAwait)) -> pure $ SomeMessage MsgWait

        (0x0a, ServerAgency (TokNext _ TokMustReply)) -> pure $ SomeMessage MsgPing

        (0x0b, ClientAgency TokPing) -> pure $ SomeMessage MsgPong

        _ -> fail $ "Unexpected tag " <> show tag
