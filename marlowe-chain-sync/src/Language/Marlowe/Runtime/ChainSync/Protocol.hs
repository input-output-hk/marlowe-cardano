{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.ChainSync.Protocol where

import Data.Binary (Binary, Get, Put, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Data.These (These (..))
import Data.Void (Void)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Types (BlockHeader, ChainPoint, Transaction, TxId, TxOutRef)
import Network.Protocol.ChainSeek.Client (ChainSeekClient)
import Network.Protocol.ChainSeek.Codec (DeserializeError, SomeQuery (..), codecChainSeek)
import Network.Protocol.ChainSeek.Server (ChainSeekServer)
import Network.Protocol.ChainSeek.Types (ChainSeek, SchemaVersion (SchemaVersion))
import Network.TypedProtocol.Codec (Codec)
import Numeric.Natural (Natural)

data Move err result where

  Fork
    :: Move err1 result1
    -> Move err2 result2
    -> Move (These err1 err2) (These result1 result2)

  AdvanceSlots :: Natural -> Move Void ()

  AdvanceBlocks :: Natural -> Move Void ()

  Intersect :: [BlockHeader] -> Move IntersectError ()

  ConsumeUTxO :: TxOutRef -> Move UTxOError Transaction

data MoveResult err result
  = RollForward result BlockHeader ChainPoint
  | RollBack ChainPoint ChainPoint
  | Reject err ChainPoint
  | Wait ChainPoint

data UTxOError
  = UTxONotFound
  | UTxOSpent TxId
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

data IntersectError = IntersectionNotFound
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

type RuntimeChainSeek = ChainSeek Move ChainPoint ChainPoint

type RuntimeChainSeekClient = ChainSeekClient Move ChainPoint ChainPoint

type RuntimeChainSeekServer = ChainSeekServer Move ChainPoint ChainPoint

type RuntimeChainSeekCodec m = Codec RuntimeChainSeek DeserializeError m LBS.ByteString

runtimeChainSeekCodec :: Applicative m => RuntimeChainSeekCodec m
runtimeChainSeekCodec = codecChainSeek putMove getMove putResult getResult putError getError put get put get

putMove :: SomeQuery Move -> Put
putMove (SomeQuery move) = case move of
  Fork m1 m2 -> do
    putWord8 0x01
    putMove $ SomeQuery m1
    putMove $ SomeQuery m2

  AdvanceSlots slots -> do
    putWord8 0x02
    put slots

  AdvanceBlocks blocks -> do
    putWord8 0x03
    put blocks

  ConsumeUTxO txOutRef -> do
    putWord8 0x04
    put txOutRef

  Intersect points -> do
    putWord8 0x05
    put points

getMove :: Get (SomeQuery Move)
getMove = do
  tag <- getWord8
  case tag of
    0x01 -> do
      SomeQuery m1 <- getMove
      SomeQuery m2 <- getMove
      pure $ SomeQuery $ Fork m1 m2
    0x02 -> SomeQuery . AdvanceSlots <$> get
    0x03 -> SomeQuery . AdvanceBlocks <$> get
    0x04 -> SomeQuery . ConsumeUTxO <$> get
    0x05 -> SomeQuery . Intersect <$> get
    _ -> fail $ "Invalid move tag " <> show tag

putResult :: forall err result. Move err result -> result -> Put
putResult = \case
  Fork m1 m2 -> \case
    This r1 -> do
      putWord8 0x01
      putResult m1 r1
    That r2 -> do
      putWord8 0x02
      putResult m2 r2
    These r1 r2 -> do
      putWord8 0x03
      putResult m1 r1
      putResult m2 r2
  AdvanceSlots _ -> mempty
  AdvanceBlocks _ -> mempty
  ConsumeUTxO _ -> put
  Intersect _ -> mempty

getResult :: forall err result. Move err result -> Get result
getResult = \case
  Fork m1 m2    -> do
    tag <- getWord8
    case tag of
      0x01 -> This <$> getResult m1
      0x02 -> That <$> getResult m2
      0x03 -> These <$> getResult m1 <*> getResult m2
      _    -> fail $ "Invalid align result tag " <> show tag
  AdvanceSlots _ -> get
  AdvanceBlocks _ -> get
  ConsumeUTxO _ -> get
  Intersect _ -> get

putError :: forall err result. Move err result -> err -> Put
putError = \case
  Fork m1 m2 -> \case
    This e1 -> do
      putWord8 0x01
      putError m1 e1
    That e2 -> do
      putWord8 0x02
      putError m2 e2
    These e1 e2 -> do
      putWord8 0x03
      putError m1 e1
      putError m2 e2
  AdvanceSlots _ -> put
  AdvanceBlocks _ -> put
  ConsumeUTxO _ -> put
  Intersect _ -> put

getError :: forall err result. Move err result -> Get err
getError = \case
  Fork m1 m2    -> do
    tag <- getWord8
    case tag of
      0x01 -> This <$> getError m1
      0x02 -> That <$> getError m2
      0x03 -> These <$> getError m1 <*> getError m2
      _    -> fail $ "Invalid fork error tag " <> show tag
  AdvanceSlots _ -> get
  AdvanceBlocks _ -> get
  ConsumeUTxO _ -> get
  Intersect _ -> get

schemaVersion1_0 :: SchemaVersion
schemaVersion1_0 = SchemaVersion "marlowe-chain-sync-1.0"
