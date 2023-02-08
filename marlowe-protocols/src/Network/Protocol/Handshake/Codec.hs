{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Handshake.Codec
  where

import Control.Monad ((<=<))
import Data.Binary
import Data.Binary.Put (runPut)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (Foldable(fold))
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
    ClientAgency (TokLiftClient tok) -> \case
      MsgLift msg -> fold
        [ runPut $ putWord8 0x3
        , encodeMsg (ClientAgency tok) msg
        ]

    ServerAgency TokHandshake -> runPut . \case
      MsgAccept -> putWord8 0x01
      MsgReject -> putWord8 0x02

    ServerAgency (TokLiftServer tok) -> \case
      MsgLift msg -> fold
        [ runPut $ putWord8 0x3
        , encodeMsg (ServerAgency tok) msg
        ]
  )
  (\tok -> decodeGet getWord8 >>= handleTag tok)
  where
    handleTag
      :: PeerHasAgency pr (st :: Handshake ps)
      -> DecodeStep LBS.ByteString DeserializeError m Word8
      -> m (DecodeStep LBS.ByteString DeserializeError m (SomeMessage st))
    handleTag tok = \case
      DecodeFail f -> pure $ DecodeFail f
      DecodePartial next -> pure $ DecodePartial $ handleTag tok <=< next
      DecodeDone tag mUnconsumed -> case tag of
        0x00 -> case tok of
          ClientAgency TokInit -> decodeSignature mUnconsumed <$> decodeGet get
          _ -> failInvalidTag "Invalid protocol state for MsgHandshake" mUnconsumed
        0x01 -> case tok of
          ServerAgency TokHandshake -> pure $ DecodeDone (SomeMessage MsgAccept) mUnconsumed
          _ -> failInvalidTag "Invalid protocol state for MsgAccept" mUnconsumed
        0x02 -> case tok of
          ServerAgency TokHandshake -> pure $ DecodeDone (SomeMessage MsgReject) mUnconsumed
          _ -> failInvalidTag "Invalid protocol state for MsgReject" mUnconsumed
        0x03 -> case tok of
          ClientAgency (TokLiftClient tok') -> handleMsg mUnconsumed <$> decodeMsg (ClientAgency tok')
          ServerAgency (TokLiftServer tok') -> handleMsg mUnconsumed <$> decodeMsg (ServerAgency tok')
          _ -> failInvalidTag "Invalid protocol state for MsgLift" mUnconsumed
        _ -> failInvalidTag ("Invalid msg tag " <> show tag) mUnconsumed
      where
        failInvalidTag message mUnconsumed = pure $ DecodeFail DeserializeError
          { message
          , offset = 0
          , unconsumedInput = foldMap toStrict mUnconsumed
          }

    decodeSignature
      :: Maybe LBS.ByteString
      -> DecodeStep LBS.ByteString DeserializeError m (Signature ps)
      -> DecodeStep LBS.ByteString DeserializeError m (SomeMessage ('StInit st' :: Handshake ps))
    decodeSignature mUnconsumed = \case
      DecodeFail f -> DecodeFail f
      DecodePartial next -> DecodePartial \mUnconsumed' -> decodeSignature Nothing <$> next (mUnconsumed <> mUnconsumed')
      DecodeDone sig mUnconsumed' -> DecodeDone (SomeMessage $ MsgHandshake sig) $ mUnconsumed <> mUnconsumed'

    handleMsg
      :: Maybe LBS.ByteString
      -> DecodeStep LBS.ByteString DeserializeError m (SomeMessage st')
      -> DecodeStep LBS.ByteString DeserializeError m (SomeMessage ('StLift st'))
    handleMsg mUnconsumed = \case
      DecodeFail f -> DecodeFail f
      DecodePartial next -> DecodePartial \mUnconsumed' -> handleMsg Nothing <$> next (mUnconsumed <> mUnconsumed')
      DecodeDone (SomeMessage msg) mUnconsumed' -> DecodeDone (SomeMessage $ MsgLift msg) $ mUnconsumed <> mUnconsumed'
