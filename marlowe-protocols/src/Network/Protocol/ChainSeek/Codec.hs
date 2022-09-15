{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.ChainSeek.Codec (DeserializeError, codecChainSeek) where

import Data.Binary
import qualified Data.ByteString.Lazy as LBS
import Data.Type.Equality (type (:~:)(Refl))
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Codec (DeserializeError, GetMessage, PutMessage, binaryCodec)
import Network.TypedProtocol.Codec
import Unsafe.Coerce (unsafeCoerce)

codecChainSeek
  :: forall query point tip m
   . ( Applicative m
     , Query query
     , Binary point
     , Binary tip
     )
  => Codec (ChainSeek query point tip) DeserializeError m LBS.ByteString
codecChainSeek = binaryCodec putMsg getMsg
  where
    putMsg :: PutMessage (ChainSeek query point tip)
    putMsg (ClientAgency TokInit) msg = case msg of
      MsgRequestHandshake schemaVersion -> do
        putWord8 0x01
        put schemaVersion

    putMsg (ClientAgency TokIdle) msg = case msg of
      MsgQueryNext query -> do
        putWord8 0x04
        let tag = tagFromQuery query
        putTag tag
        putQuery query
      MsgDone            -> putWord8 0x09

    putMsg (ServerAgency TokHandshake) msg = case msg of
      MsgConfirmHandshake                  -> putWord8 0x02
      MsgRejectHandshake supportedVersions -> do
       putWord8 0x03
       put supportedVersions

    putMsg (ServerAgency (TokNext tag _)) (MsgRejectQuery err tip) = do
        putWord8 0x05
        putTag $ coerceTag tag
        putErr (coerceTag tag) err
        put tip

    putMsg (ServerAgency (TokNext tag _)) (MsgRollForward result pos tip) = do
        putWord8 0x06
        putTag $ coerceTag tag
        putResult (coerceTag tag) result
        put pos
        put tip

    putMsg (ServerAgency TokNext{}) (MsgRollBackward pos tip) = do
        putWord8 0x07
        put pos
        put tip

    putMsg (ServerAgency TokNext{}) MsgWait = putWord8 0x08

    putMsg (ServerAgency TokNext{}) MsgPing = putWord8 0x0a

    putMsg (ClientAgency TokPing) MsgPong = putWord8 0x0b

    getMsg :: GetMessage (ChainSeek query point tip)
    getMsg tok      = do
      tag <- getWord8
      case (tag, tok) of
        (0x01, ClientAgency TokInit) -> SomeMessage . MsgRequestHandshake <$> get

        (0x04, ClientAgency TokIdle) -> do
          SomeTag qtag <- getTag
          SomeMessage . MsgQueryNext <$> getQuery qtag

        (0x09, ClientAgency TokIdle) -> pure $ SomeMessage MsgDone

        (0x02, ServerAgency TokHandshake) -> pure $ SomeMessage MsgConfirmHandshake

        (0x03, ServerAgency TokHandshake) -> SomeMessage . MsgRejectHandshake <$> get

        (0x05, ServerAgency (TokNext qtag _)) -> do
          SomeTag qtag' :: SomeTag query <- getTag
          case tagEq (coerceTag qtag) qtag' of
            Nothing -> fail "decoded query tag does not match expected query tag"
            Just (Refl, Refl) -> do
              err <- getErr qtag'
              tip <- get
              pure $ SomeMessage $ MsgRejectQuery err tip

        (0x06, ServerAgency (TokNext qtag _)) -> do
          SomeTag qtag' :: SomeTag query <- getTag
          case tagEq (coerceTag qtag) qtag' of
            Nothing -> fail "decoded query tag does not match expected query tag"
            Just (Refl, Refl) -> do
              result <- getResult qtag'
              point <- get
              tip <- get
              pure $ SomeMessage $ MsgRollForward result point tip

        (0x07, ServerAgency (TokNext _ _)) -> do
          point <- get
          tip <- get
          pure $ SomeMessage $ MsgRollBackward point tip

        (0x08, ServerAgency (TokNext _ TokCanAwait)) -> pure $ SomeMessage MsgWait

        (0x0a, ServerAgency (TokNext _ TokMustReply)) -> pure $ SomeMessage MsgPing

        (0x0b, ClientAgency TokPing) -> pure $ SomeMessage MsgPong

        _ -> fail $ "Unexpected tag " <> show tag

    -- Unfortunately, the poly-kinded query parameter doesn't play nicely with
    -- the `PeerHasAgency` type and it gets confused, thinking that 'query1'
    -- and 'query' are unrelated types. So we have to coerce them (they will
    -- absolutely be the same type constructor though).
    coerceTag :: forall query1 err result. Tag query1 err result -> Tag query err result
    coerceTag = unsafeCoerce
