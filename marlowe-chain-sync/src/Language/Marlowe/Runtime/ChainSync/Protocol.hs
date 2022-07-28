{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Protocol where

import Cardano.Api (BlockHeader (BlockHeader), BlockNo (..), ChainPoint (..), ChainTip (..), SlotNo (..))
import Cardano.Api.Shelley (Hash (..))
import Data.Binary (Get, Put, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Data.These (These (..))
import Data.Void (Void, absurd)
import Network.Protocol.FilteredChainSync.Client (FilteredChainSyncClient)
import Network.Protocol.FilteredChainSync.Codec (DeserializeError, SomeQuery (SomeQuery), codecFilteredChainSync)
import Network.Protocol.FilteredChainSync.Server (FilteredChainSyncServer)
import Network.Protocol.FilteredChainSync.Types (FilteredChainSync, SchemaVersion (SchemaVersion))
import Network.TypedProtocol.Codec (Codec)
import Numeric.Natural (Natural)

data Query err result where

  Or
    :: Query err1 result1
    -> Query err2 result2
    -> Query (These err1 err2) (These result1 result2)

  GetBlockHeader :: Query Void BlockHeader

  WaitSlots :: Natural -> Query err result -> Query err result

  WaitBlocks :: Natural -> Query err result -> Query err result

data QueryResult err result
  = RollForward result ChainPoint ChainTip
  | RollBack ChainPoint ChainTip
  | Reject err ChainTip
  | Wait ChainTip

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
  putChainPoint
  getChainPoint
  putChainTip
  getChainTip
  where
    encodeQuery :: SomeQuery Query -> Put
    encodeQuery (SomeQuery q) = case q of
      Or q1 q2 -> do
        putWord8 0x01
        encodeQuery $ SomeQuery q1
        encodeQuery $ SomeQuery q2

      GetBlockHeader -> putWord8 0x02

      WaitSlots slots query -> do
        putWord8 0x03
        put slots
        encodeQuery $ SomeQuery query

      WaitBlocks blocks query -> do
        putWord8 0x04
        put blocks
        encodeQuery $ SomeQuery query

    decodeQuery = do
      tag <- getWord8
      case tag of
        0x01 -> do
          SomeQuery q1 <- decodeQuery
          SomeQuery q2 <- decodeQuery
          pure $ SomeQuery $ Or q1 q2

        0x02 -> pure $ SomeQuery GetBlockHeader
        0x03 -> do
          slots <- get
          SomeQuery query <- decodeQuery
          let query' = WaitSlots slots query
          pure $ SomeQuery query'
        0x04 -> do
          blocks <- get
          SomeQuery query <- decodeQuery
          let query' = WaitBlocks blocks query
          pure $ SomeQuery query'
        _ -> fail $ "Invalid query tag " <> show tag

    encodeResult :: forall err result. Query err result -> result -> Put
    encodeResult = \case
      Or q1 q2 -> \case
        This r1 -> do
          putWord8 0x01
          encodeResult q1 r1
        That r2 -> do
          putWord8 0x02
          encodeResult q2 r2
        These r1 r2 -> do
          putWord8 0x03
          encodeResult q1 r1
          encodeResult q2 r2

      GetBlockHeader -> putBlockHeader

      WaitSlots _ query -> encodeResult query

      WaitBlocks _ query -> encodeResult query

    decodeResult :: forall err result. Query err result -> Get result
    decodeResult = \case
      Or q1 q2    -> do
        tag <- getWord8
        case tag of
          0x01 -> This <$> decodeResult q1
          0x02 -> That <$> decodeResult q2
          0x03 -> These <$> decodeResult q1 <*> decodeResult q2
          _    -> fail $ "Invalid align result tag " <> show tag

      GetBlockHeader -> getBlockHeader

      WaitSlots _ query -> decodeResult query

      WaitBlocks _ query -> decodeResult query

    encodeError :: forall err result. Query err result -> err -> Put
    encodeError = \case
      Or q1 q2 -> \case
        This e1 -> do
          putWord8 0x01
          encodeError q1 e1
        That e2 -> do
          putWord8 0x02
          encodeError q2 e2
        These e1 e2 -> do
          putWord8 0x03
          encodeError q1 e1
          encodeError q2 e2

      GetBlockHeader -> absurd

      WaitSlots _ query -> encodeError query

      WaitBlocks _ query -> encodeError query

    decodeError :: forall err result. Query err result -> Get err
    decodeError = \case
      Or q1 q2    -> do
        tag <- getWord8
        case tag of
          0x01 -> This <$> decodeError q1
          0x02 -> That <$> decodeError q2
          0x03 -> These <$> decodeError q1 <*> decodeError q2
          _    -> fail $ "Invalid or error tag " <> show tag

      GetBlockHeader -> fail "absurd"

      WaitSlots _ query -> decodeError query

      WaitBlocks _ query -> decodeError query

schemaVersion1_0 :: SchemaVersion
schemaVersion1_0 = SchemaVersion "marlowe-chain-sync-1.0"

putSlotNo :: SlotNo -> Put
putSlotNo (SlotNo slot) = do
  put slot

getSlotNo :: Get SlotNo
getSlotNo = SlotNo <$> get

putHeaderHash :: Hash BlockHeader -> Put
putHeaderHash (HeaderHash slot) = do
  put slot

getHeaderHash :: Get (Hash BlockHeader )
getHeaderHash = HeaderHash <$> get

putBlockNo :: BlockNo -> Put
putBlockNo (BlockNo block) = do
  put block

getBlockNo :: Get BlockNo
getBlockNo = BlockNo <$> get

putBlockHeader :: BlockHeader -> Put
putBlockHeader (BlockHeader slot hash block) = do
  putSlotNo slot
  putHeaderHash hash
  putBlockNo block

getBlockHeader :: Get BlockHeader
getBlockHeader = BlockHeader <$> getSlotNo <*> getHeaderHash <*> getBlockNo

putChainPoint :: ChainPoint -> Put
putChainPoint = \case
  ChainPointAtGenesis -> putWord8 0x01
  ChainPoint slot hash -> do
    putWord8 0x02
    putSlotNo slot
    putHeaderHash hash

getChainPoint :: Get ChainPoint
getChainPoint = do
  tag <- getWord8
  case tag of
    0x01 -> pure ChainPointAtGenesis
    0x02 -> ChainPoint <$> getSlotNo <*> getHeaderHash
    _    -> fail $ "Invalid chaoin point tag " <> show tag

putChainTip :: ChainTip -> Put
putChainTip = \case
  ChainTipAtGenesis -> putWord8 0x01
  ChainTip slot hash block -> do
    putWord8 0x02
    putSlotNo slot
    putHeaderHash hash
    putBlockNo block

getChainTip :: Get ChainTip
getChainTip = do
  tag <- getWord8
  case tag of
    0x01 -> pure ChainTipAtGenesis
    0x02 -> ChainTip <$> getSlotNo <*> getHeaderHash <*> getBlockNo
    _    -> fail $ "Invalid chaoin tip tag " <> show tag
