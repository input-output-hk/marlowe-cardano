{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetTipForContract
  where

import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader(..), BlockHeaderHash(..), ChainPoint, TxId(..), TxOutRef(..), WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..))

getTipForContract :: ContractId -> T.Transaction ChainPoint
getTipForContract (ContractId TxOutRef{..}) = T.statement params $ decodePoint <$>
  [maybeStatement|
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
    SELECT
      slotNo :: bigint,
      blockId :: bytea,
      blockNo :: bigint
    FROM marlowe.applyTx
    JOIN contractId
      ON applyTx.createTxId = contractId.txId
        AND applyTx.createTxIx = contractId.txIx
    UNION
    SELECT
      slotNo :: bigint,
      blockId :: bytea,
      blockNo :: bigint
    FROM marlowe.withdrawalTxIn
    JOIN contractId
      ON withdrawalTxIn.createTxId = contractId.txId
        AND withdrawalTxIn.createTxIx = contractId.txIx
    ORDER BY slotNo DESC
    LIMIT 1
  |]
  where
    params = (unTxId txId, fromIntegral txIx)
    decodePoint = \case
      Nothing -> Genesis
      Just (slot, hash, block) -> At $ BlockHeader
        (fromIntegral slot)
        (BlockHeaderHash hash)
        (fromIntegral block)
