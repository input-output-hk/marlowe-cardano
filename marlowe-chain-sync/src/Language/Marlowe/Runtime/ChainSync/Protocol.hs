{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs     #-}

module Language.Marlowe.Runtime.ChainSync.Protocol where

import Cardano.Api (BlockNo (..), ChainPoint (..), ChainTip (..), SlotNo (..))
import Cardano.Api.Shelley (Hash (..))
import Data.Binary (Get, Put, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Network.Protocol.FilteredChainSync.Client (FilteredChainSyncClient)
import Network.Protocol.FilteredChainSync.Codec (DeserializeError, SomeQuery (SomeQuery), codecFilteredChainSync)
import Network.Protocol.FilteredChainSync.Server (FilteredChainSyncServer)
import Network.Protocol.FilteredChainSync.Types (FilteredChainSync, SchemaVersion (SchemaVersion))
import Network.TypedProtocol.Codec (Codec)

data Query err result where

type RuntimeFilteredChainSync = FilteredChainSync Query ChainPoint ChainTip

type RuntimeFilteredChainSyncClient = FilteredChainSyncClient Query ChainPoint ChainTip

type RuntimeFilteredChainSyncServer = FilteredChainSyncServer Query ChainPoint ChainTip

type RuntimeFilteredChainSyncCodec m = Codec RuntimeFilteredChainSync DeserializeError m LBS.ByteString

runtimeFilteredChainSyncCodec :: Applicative m => RuntimeFilteredChainSyncCodec m
runtimeFilteredChainSyncCodec = codecFilteredChainSync
  encodeQuery
  decodeQuery
  encodeResult
  decodeResult
  encodeError
  decodeError
  encodePoint
  decodePoint
  encodeTip
  decodeTip
  where
    encodeQuery :: SomeQuery Query -> Put
    encodeQuery (SomeQuery q) = case q of {}

    decodeQuery = fail "no queries defined"

    encodeResult :: forall err result. Query err result -> result -> Put
    encodeResult = \case

    decodeResult :: forall err result. Query err result -> Get result
    decodeResult = \case

    encodeError :: forall err result. Query err result -> err -> Put
    encodeError = \case

    decodeError :: forall err result. Query err result -> Get err
    decodeError = \case

    encodePoint = \case
      ChainPointAtGenesis -> putWord8 0x01
      ChainPoint (SlotNo slot) (HeaderHash hash) -> do
        putWord8 0x02
        put slot
        put hash

    decodePoint = do
      tag <- getWord8
      case tag of
        0x01 -> pure ChainPointAtGenesis
        0x02 -> do
          slot <- get
          ChainPoint (SlotNo slot) . HeaderHash <$> get
        _ -> fail $ "Invalid chaoin point tag " <> show tag

    encodeTip = \case
      ChainTipAtGenesis -> putWord8 0x01
      ChainTip (SlotNo slot) (HeaderHash hash) (BlockNo block) -> do
        putWord8 0x02
        put slot
        put hash
        put block

    decodeTip = do
      tag <- getWord8
      case tag of
        0x01 -> pure ChainTipAtGenesis
        0x02 -> do
          slot <- get
          hash <- get
          block <- get
          pure $ ChainTip (SlotNo slot) (HeaderHash hash) (BlockNo block)
        _ -> fail $ "Invalid chaoin tip tag " <> show tag

schemaVersion1_0 :: SchemaVersion
schemaVersion1_0 = SchemaVersion "marlowe-chain-sync-1.0"
