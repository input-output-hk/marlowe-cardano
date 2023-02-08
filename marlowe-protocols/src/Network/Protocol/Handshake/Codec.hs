{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
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
  :: forall ps h m
   . (Monad m, Binary h)
  => Codec ps DeserializeError m LBS.ByteString
  -> Codec (Handshake h ps) DeserializeError m LBS.ByteString
codecHandshake (Codec encodeMsg decodeMsg) = Codec
  ( \case
    ClientAgency TokInit -> \case
      MsgHandshake h -> runPut do
        putWord8 0x00
        put h
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
      :: PeerHasAgency pr (st :: Handshake h ps)
      -> DecodeStep LBS.ByteString DeserializeError m Word8
      -> m (DecodeStep LBS.ByteString DeserializeError m (SomeMessage st))
    handleTag tok = \case
      DecodeFail f -> pure $ DecodeFail f
      DecodePartial next -> pure $ DecodePartial $ handleTag tok <=< next
      DecodeDone tag mUnconsumed -> case tag of
        0x00 -> case tok of
          ClientAgency TokInit -> handleH <$> decodeGet get
          _ -> failInvalidTag "Invalid protocol state for MsgHandshake" mUnconsumed
        0x01 -> case tok of
          ServerAgency TokHandshake -> pure $ DecodeDone (SomeMessage MsgAccept) mUnconsumed
          _ -> failInvalidTag "Invalid protocol state for MsgAccept" mUnconsumed
        0x02 -> case tok of
          ServerAgency TokHandshake -> pure $ DecodeDone (SomeMessage MsgAccept) mUnconsumed
          _ -> failInvalidTag "Invalid protocol state for MsgAccept" mUnconsumed
        0x03 -> case tok of
          ClientAgency (TokLiftClient tok') -> handleMsg <$> decodeMsg (ClientAgency tok')
          ServerAgency (TokLiftServer tok') -> handleMsg <$> decodeMsg (ServerAgency tok')
          _ -> failInvalidTag "Invalid protocol state for MsgLift" mUnconsumed
        _ -> failInvalidTag ("Invalid msg tag " <> show tag) mUnconsumed
      where
        failInvalidTag message mUnconsumed = pure $ DecodeFail DeserializeError
          { message
          , offset = 0
          , unconsumedInput = foldMap toStrict mUnconsumed
          }

    handleH
      :: DecodeStep LBS.ByteString DeserializeError m h
      -> DecodeStep LBS.ByteString DeserializeError m (SomeMessage ('StInit st' :: Handshake h ps))
    handleH = \case
      DecodeFail f -> DecodeFail f
      DecodePartial next -> DecodePartial $ fmap handleH <$> next
      DecodeDone h mUnconsumed -> DecodeDone (SomeMessage $ MsgHandshake h) mUnconsumed

    handleMsg
      :: DecodeStep LBS.ByteString DeserializeError m (SomeMessage st')
      -> DecodeStep LBS.ByteString DeserializeError m (SomeMessage ('StLift st'))
    handleMsg = \case
      DecodeFail f -> DecodeFail f
      DecodePartial next -> DecodePartial $ fmap handleMsg <$> next
      DecodeDone (SomeMessage msg) mUnconsumed -> DecodeDone (SomeMessage $ MsgLift msg) mUnconsumed
