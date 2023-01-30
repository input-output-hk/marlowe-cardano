{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetIntersectionForContract
  where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import qualified Data.Vector as V
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader(..), BlockHeaderHash(..), TxId(..), TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(ContractId), MarloweVersion(..), SomeMarloweVersion(..))

getIntersectionForContract :: ContractId -> [BlockHeader] -> T.Transaction (Maybe (BlockHeader, SomeMarloweVersion))
getIntersectionForContract _ [] = pure Nothing
getIntersectionForContract (ContractId TxOutRef{..}) (b : bs) = do
  serverBlocks <- fmap decodeBlock . V.toList <$> T.statement params
    [vectorStatement|
      WITH contractId (txId, txIx) AS
        ( SELECT $1 :: bytea, $2 :: smallint
        )
      SELECT
        slotNo :: bigint,
        blockId :: bytea,
        blockNo :: bigint
      FROM marlowe.createTxOut
      JOIN contractId USING (txId, txIx)
      UNION
      SELECT DISTINCT
        slotNo :: bigint,
        blockId :: bytea,
        blockNo :: bigint
      FROM marlowe.applyTx
      JOIN contractId
        ON contractId.txId = applyTx.createTxId
        AND contractId.txIx = applyTx.createTxIx
      UNION
      SELECT DISTINCT
        slotNo :: bigint,
        blockId :: bytea,
        blockNo :: bigint
      FROM marlowe.withdrawalTxIn
      JOIN contractId
        ON contractId.txId = withdrawalTxIn.createTxId
        AND contractId.txIx = withdrawalTxIn.createTxIx
      ORDER BY slotNo
    |]
  pure
    $ fmap ((, SomeMarloweVersion MarloweV1) . fst)
    $ listToMaybe
    $ reverse
    $ takeWhile (uncurry (==))
    $ zip (b : bs)
    $ dropWhile (/= b) serverBlocks
  where
    params = (unTxId txId, fromIntegral txIx)

decodeBlock :: (Int64, ByteString, Int64) -> BlockHeader
decodeBlock (slot, hash, block) = BlockHeader
  (fromIntegral slot)
  (BlockHeaderHash hash)
  (fromIntegral block)
