{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Protocol where

import Cardano.Api (AsType (..), BlockHeader (..), BlockNo (..), ChainPoint (..), ChainTip (..),
                    SerialiseAsRawBytes (..), SlotNo (..), TxId, TxIx (..))
import Cardano.Api.Shelley (Hash (..))
import Control.Monad (replicateM)
import Data.Binary (Get, Put, get, getWord8, put, putWord8)
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Data.These (These (..))
import Data.Void (Void, absurd)
import Network.Protocol.FilteredChainSync.Client (FilteredChainSyncClient)
import Network.Protocol.FilteredChainSync.Codec (DeserializeError, SomeQuery (..), codecFilteredChainSync)
import Network.Protocol.FilteredChainSync.Server (FilteredChainSyncServer)
import Network.Protocol.FilteredChainSync.Types (FilteredChainSync, SchemaVersion (SchemaVersion))
import Network.TypedProtocol.Codec (Codec)
import Numeric.Natural (Natural)

data WaitUntilUTxOSpentError
  = UTxONotFound
  | UTxOAlreadySpent TxId

data IntersectError = IntersectionNotFound

data Move err result where

  Or
    :: Move err1 result1
    -> Move err2 result2
    -> Move (These err1 err2) (These result1 result2)

  Extract :: Extract err result -> Move err result

  WaitSlots :: Natural -> Move err result -> Move err result

  WaitBlocks :: Natural -> Move err result -> Move err result

  WaitUntilUTxOSpent :: TxId -> TxIx -> Move err result -> Move (Either WaitUntilUTxOSpentError err) result

  Intersect :: [ChainPoint] -> Move err result -> Move (Either IntersectError err) result

data Extract err result where

  And
    :: Extract err1 result1
    -> Extract err2 result2
    -> Extract (These err1 err2) (result1, result2)

  GetBlockHeader :: Extract Void BlockHeader

  Nil :: Extract Void ()

data SomeExtract = forall err result. SomeExtract (Extract err result)

data MoveResult err result
  = RollForward result ChainPoint ChainTip
  | RollBack ChainPoint ChainTip
  | Reject err ChainTip
  | Wait ChainTip

type RuntimeFilteredChainSync = FilteredChainSync Move ChainPoint ChainTip

type RuntimeFilteredChainSyncClient = FilteredChainSyncClient Move ChainPoint ChainTip

type RuntimeFilteredChainSyncServer = FilteredChainSyncServer Move ChainPoint ChainTip

type RuntimeFilteredChainSyncCodec m = Codec RuntimeFilteredChainSync DeserializeError m LBS.ByteString

runtimeFilteredChainSyncCodec :: Applicative m => RuntimeFilteredChainSyncCodec m
runtimeFilteredChainSyncCodec = codecFilteredChainSync
  putMove
  getMove
  encodeResult
  decodeResult
  encodeError
  decodeError
  putChainPoint
  getChainPoint
  putChainTip
  getChainTip
  where
    putMove :: SomeQuery Move -> Put
    putMove (SomeQuery move) = case move of
      Or m1 m2 -> do
        putWord8 0x01
        putMove $ SomeQuery m1
        putMove $ SomeQuery m2

      Extract extract -> do
        putWord8 0x02
        putExtract extract

      WaitSlots slots move' -> do
        putWord8 0x03
        put slots
        putMove $ SomeQuery move'

      WaitBlocks blocks move' -> do
        putWord8 0x04
        put blocks
        putMove $ SomeQuery move'

      WaitUntilUTxOSpent txId txIx move' -> do
        putWord8 0x05
        putTxId txId
        putTxIx txIx
        putMove $ SomeQuery move'

      Intersect points move' -> do
        putWord8 0x06
        put $ length points
        traverse_ putChainPoint points
        putMove $ SomeQuery move'

    getMove = do
      tag <- getWord8
      case tag of
        0x01 -> do
          SomeQuery m1 <- getMove
          SomeQuery m2 <- getMove
          pure $ SomeQuery $ Or m1 m2

        0x02 -> do
          SomeExtract extract <- getExtract
          pure $ SomeQuery $ Extract extract
        0x03 -> do
          slots <- get
          SomeQuery move <- getMove
          pure $ SomeQuery $ WaitSlots slots move
        0x04 -> do
          blocks <- get
          SomeQuery move <- getMove
          pure $ SomeQuery $ WaitBlocks blocks move
        0x05 -> do
          txId <- getTxId
          txIx <- getTxIx
          SomeQuery move <- getMove
          pure $ SomeQuery $ WaitUntilUTxOSpent txId txIx move
        0x06 -> do
          n <- get
          points <- replicateM n getChainPoint
          SomeQuery move <- getMove
          pure $ SomeQuery $ Intersect points move
        _ -> fail $ "Invalid move tag " <> show tag

    encodeResult :: forall err result. Move err result -> result -> Put
    encodeResult = \case
      Or m1 m2 -> \case
        This r1 -> do
          putWord8 0x01
          encodeResult m1 r1
        That r2 -> do
          putWord8 0x02
          encodeResult m2 r2
        These r1 r2 -> do
          putWord8 0x03
          encodeResult m1 r1
          encodeResult m2 r2

      Extract extract -> putExtractResult extract

      WaitSlots _ move -> encodeResult move

      WaitBlocks _ move -> encodeResult move

      WaitUntilUTxOSpent _ _ move -> encodeResult move

      Intersect _ move -> encodeResult move

    decodeResult :: forall err result. Move err result -> Get result
    decodeResult = \case
      Or m1 m2    -> do
        tag <- getWord8
        case tag of
          0x01 -> This <$> decodeResult m1
          0x02 -> That <$> decodeResult m2
          0x03 -> These <$> decodeResult m1 <*> decodeResult m2
          _    -> fail $ "Invalid align result tag " <> show tag

      Extract extract -> getExtractResult extract

      WaitSlots _ move -> decodeResult move

      WaitBlocks _ move -> decodeResult move

      WaitUntilUTxOSpent _ _ move -> decodeResult move

      Intersect _ move -> decodeResult move

    encodeError :: forall err result. Move err result -> err -> Put
    encodeError = \case
      Or m1 m2 -> \case
        This e1 -> do
          putWord8 0x01
          encodeError m1 e1
        That e2 -> do
          putWord8 0x02
          encodeError m2 e2
        These e1 e2 -> do
          putWord8 0x03
          encodeError m1 e1
          encodeError m2 e2

      Extract extract -> putExtractError extract

      WaitSlots _ move -> encodeError move

      WaitBlocks _ move -> encodeError move

      WaitUntilUTxOSpent _ _ move -> \case
        Left spendErr -> do
          putWord8 0x01
          case spendErr of
            UTxONotFound -> putWord8 0x01
            UTxOAlreadySpent txId -> do
              putWord8 0x02
              putTxId txId
        Right err -> do
          putWord8 0x02
          encodeError move err

      Intersect _ move -> \case
        Left _ -> do
          putWord8 0x01
        Right err -> do
          putWord8 0x02
          encodeError move err

    decodeError :: forall err result. Move err result -> Get err
    decodeError = \case
      Or m1 m2    -> do
        tag <- getWord8
        case tag of
          0x01 -> This <$> decodeError m1
          0x02 -> That <$> decodeError m2
          0x03 -> These <$> decodeError m1 <*> decodeError m2
          _    -> fail $ "Invalid or error tag " <> show tag

      Extract extract -> getExtractError extract

      WaitSlots _ move -> decodeError move

      WaitBlocks _ move -> decodeError move

      WaitUntilUTxOSpent _ _ move -> do
        tag <- getWord8
        case tag of
          0x01 -> Left <$> do
            tag' <- getWord8
            case tag' of
              0x01 -> pure UTxONotFound
              0x02 -> UTxOAlreadySpent <$> getTxId
              _    -> fail $ "Invalid WaitUntilUTxOSpentError tag " <> show tag
          0x02 -> Right <$> decodeError move
          _    -> fail $ "Invalid Either tag " <> show tag

      Intersect _ move -> do
        tag <- getWord8
        case tag of
          0x01 -> pure $ Left IntersectionNotFound
          0x02 -> Right <$> decodeError move
          _    -> fail $ "Invalid Either tag " <> show tag

schemaVersion1_0 :: SchemaVersion
schemaVersion1_0 = SchemaVersion "marlowe-chain-sync-1.0"

putExtract :: Extract err result -> Put
putExtract = \case
  And e1 e2 -> do
    putWord8 0x01
    putExtract e1
    putExtract e2
  GetBlockHeader -> putWord8 0x02
  Nil -> putWord8 0x03

putExtractResult :: Extract err result -> result -> Put
putExtractResult = \case
  And e1 e2 -> \(r1, r2) -> do
    putExtractResult e1 r1
    putExtractResult e2 r2
  GetBlockHeader -> putBlockHeader
  Nil -> const mempty

putExtractError :: Extract err result -> err -> Put
putExtractError = \case
  And e1 e2 -> \case
    This err1 -> do
      putWord8 0x01
      putExtractError e1 err1
    That err2 -> do
      putWord8 0x02
      putExtractError e2 err2
    These err1 err2 -> do
      putWord8 0x03
      putExtractError e1 err1
      putExtractError e2 err2
  GetBlockHeader -> absurd
  Nil -> absurd

getExtract :: Get SomeExtract
getExtract = do
  tag <- getWord8
  case tag of
    0x01 -> do
      SomeExtract e1 <- getExtract
      SomeExtract e2 <- getExtract
      pure $ SomeExtract $ And e1 e2
    0x02 -> pure $ SomeExtract GetBlockHeader
    0x03 -> pure $ SomeExtract Nil
    _ -> fail $ "Invalid extract tag " <> show tag

getExtractResult :: Extract err result -> Get result
getExtractResult = \case
  And e1 e2      -> (,) <$> getExtractResult e1 <*> getExtractResult e2
  GetBlockHeader -> getBlockHeader
  Nil            -> pure ()

getExtractError :: Extract err result -> Get err
getExtractError = \case
  And e1 e2    -> do
    tag <- getWord8
    case tag of
      0x01 -> This <$> getExtractError e1
      0x02 -> That <$> getExtractError e2
      0x03 -> These <$> getExtractError e1 <*> getExtractError e2
      _    -> fail $ "Invalid or extract error tag " <> show tag

  GetBlockHeader -> fail "absurd"
  Nil -> fail "absurd"

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

putTxId :: TxId -> Put
putTxId = put . serialiseToRawBytes

getTxId :: Get TxId
getTxId = do
  bytes <- get
  case deserialiseFromRawBytes AsTxId bytes of
    Nothing   -> fail $ T.unpack $ "Invalid TxId bytes: " <> encodeBase16 bytes
    Just txId -> pure txId

putTxIx :: TxIx -> Put
putTxIx (TxIx ix) = put ix

getTxIx :: Get TxIx
getTxIx  = TxIx <$> get
