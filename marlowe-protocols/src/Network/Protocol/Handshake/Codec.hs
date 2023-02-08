{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Handshake.Codec
  where

import Control.Monad (unless)
import Data.Binary
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.Codec (DeserializeError(..), decodeGet)
import Network.Protocol.Handshake.Types
import Network.TypedProtocol.Codec

codecHandshake
  :: forall ps m
   . (Monad m, Binary (Signature ps))
  => Codec ps DeserializeError m LBS.ByteString
  -> Codec (Handshake ps) DeserializeError m LBS.ByteString
codecHandshake (Codec encodeMsg decodeMsg) = Codec
  ( \case
    ClientAgency TokInit -> \case
      MsgHandshake sig -> runPut do
        putWord8 0x00
        put sig
    ServerAgency TokHandshake -> runPut . \case
      MsgAccept -> putWord8 0x01
      MsgReject -> putWord8 0x02
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> encodeMsg (ClientAgency tok) msg
    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> encodeMsg (ServerAgency tok) msg
  )
  ( \case
    ClientAgency TokInit -> decodeGet do
      tag <- getWord8
      unless (tag == 0x00) $ fail $ "Expected 0x00, got " <> show tag
      SomeMessage . MsgHandshake <$> get
    ServerAgency TokHandshake -> decodeGet do
      tag <- getWord8
      case tag of
        0x01 -> pure $ SomeMessage MsgAccept
        0x02 -> pure $ SomeMessage MsgReject
        _ -> fail $ "Expected 0x01 or 0x02, got " <> show tag
    ClientAgency (TokLiftClient tok) -> handleMsg <$> decodeMsg (ClientAgency tok)
    ServerAgency (TokLiftServer tok) -> handleMsg <$> decodeMsg (ServerAgency tok)
  )
  where
    handleMsg
      :: DecodeStep LBS.ByteString DeserializeError m (SomeMessage st')
      -> DecodeStep LBS.ByteString DeserializeError m (SomeMessage ('StLift st'))
    handleMsg = \case
      DecodeFail f -> DecodeFail f
      DecodePartial next -> DecodePartial $ fmap handleMsg . next
      DecodeDone (SomeMessage msg) unconsumed -> DecodeDone (SomeMessage $ MsgLift msg) unconsumed
