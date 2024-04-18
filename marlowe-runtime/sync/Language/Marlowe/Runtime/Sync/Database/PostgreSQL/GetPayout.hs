{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayout where

import Data.ByteString (ByteString)
import Data.Int (Int16, Int64)
import Data.Vector (Vector)
import Hasql.TH (maybeStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types (
  PayoutState (..),
  SomePayoutState (..),
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  TxId (..),
  TxOutRef (..),
  unTxIx,
 )
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetContractState (
  decodeContractId,
  decodePayout,
  decodeTxOutRef,
 )
import Prelude hiding (init)

-- | Fetch a payout by its ID.
getPayout
  :: TxOutRef
  -- ^ The tx output that sent the payout, which identifies the payout.
  -> T.Transaction (Maybe SomePayoutState)
getPayout TxOutRef{..} =
  fmap decodeSomePayoutState
    <$> T.statement
      (unTxId txId, fromIntegral $ unTxIx txIx)
      [maybeStatement|
          SELECT
            (ARRAY_AGG(applyTx.createTxId))[1] :: bytea,
            (ARRAY_AGG(applyTx.createTxIx))[1] :: smallint,
            payoutTxOut.txId :: bytea,
            payoutTxOut.txIx :: smallint,
            (ARRAY_AGG(payoutTxOut.rolesCurrency))[1] :: bytea,
            (ARRAY_AGG(payoutTxOut.role))[1] :: bytea,
            (ARRAY_AGG(txOut.address))[1] :: bytea,
            (ARRAY_AGG(txOut.lovelace))[1] :: bigint,
            ARRAY_REMOVE(ARRAY_AGG(txOutAsset.policyId), NULL) :: bytea[],
            ARRAY_REMOVE(ARRAY_AGG(txOutAsset.name), NULL) :: bytea[],
            ARRAY_REMOVE(ARRAY_AGG(txOutAsset.quantity), NULL) :: bigint[],
            (ARRAY_AGG(withdrawalTxIn.txId))[1] :: bytea?
          FROM marlowe.payoutTxOut
          NATURAL JOIN marlowe.txOut
          NATURAL LEFT JOIN marlowe.txOutAsset
          NATURAL JOIN marlowe.applyTx
          LEFT JOIN marlowe.withdrawalTxIn
            ON withdrawalTxIn.payoutTxId = payoutTxOut.txId
            AND withdrawalTxIn.payoutTxIx = payoutTxOut.txIx
          WHERE payoutTxOut.txId = $1 :: bytea
            AND payoutTxOut.txIx = $2 :: smallint
          GROUP BY payoutTxOut.txId, payoutTxOut.txIx
        |]

decodeSomePayoutState
  :: ( ByteString
     , Int16
     , ByteString
     , Int16
     , ByteString
     , ByteString
     , ByteString
     , Int64
     , Vector ByteString
     , Vector ByteString
     , Vector Int64
     , Maybe ByteString
     )
  -> SomePayoutState
decodeSomePayoutState
  ( createTxId
    , createTxIx
    , txId
    , txIx
    , rolesCurrency
    , role
    , address
    , lovelace
    , policyIds
    , tokenNames
    , quantities
    , withdrawalTxId
    ) =
    SomePayoutState
      MarloweV1
      PayoutState
        { contractId = decodeContractId createTxId createTxIx
        , payoutId = decodeTxOutRef txId txIx
        , payout =
            snd $
              decodePayout (txId, txIx, rolesCurrency, role, address, lovelace, policyIds, tokenNames, quantities)
        , withdrawalId = TxId <$> withdrawalTxId
        }
