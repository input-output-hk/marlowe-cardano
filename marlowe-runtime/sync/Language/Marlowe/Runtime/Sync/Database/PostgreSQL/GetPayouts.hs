{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetPayouts where

import Control.Monad (guard)
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Hasql.TH (vectorStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types (
  Order (..),
  Page (..),
  PayoutFilter (..),
  PayoutRef (..),
  Range (..),
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  AssetId (..),
  TxOutRef (..),
 )
import Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetWithdrawal (decodePayoutRef)
import Prelude hiding (init)

-- | Fetch a page of payouts for a given filter and range.
getPayouts
  :: PayoutFilter
  -- ^ The filter, which controls which payouts are included in the result set.
  -> Range TxOutRef
  -- ^ The page range, which controls which results from the result set are returned, and in what order.
  -> T.Transaction (Maybe (Page TxOutRef PayoutRef))
getPayouts PayoutFilter{..} Range{..} = do
  -- FIXME this is a temporary, limited and memory-intensive implementation that needs to be replaced with dynamic SQL.
  allPayouts <-
    V.toList . fmap (uncurry6 decodePayoutRef)
      <$> T.statement
        ()
        if unclaimed
          then
            [vectorStatement|
              SELECT
                applyTx.createTxId :: bytea,
                applyTx.createTxIx :: smallint,
                payoutTxOut.txId :: bytea,
                payoutTxOut.txIx :: smallint,
                payoutTxOut.rolesCurrency :: bytea,
                payoutTxOut.role :: bytea
              FROM marlowe.payoutTxOut
              NATURAL JOIN marlowe.applyTx
              LEFT JOIN marlowe.withdrawalTxIn
                ON payoutTxOut.txId = withdrawalTxIn.payoutTxId
                AND payoutTxOut.txIx = withdrawalTxIn.payoutTxIx
              WHERE withdrawalTxIn.txId IS NULL
              ORDER BY applyTx.slotNo, payoutTxOut.txId, payoutTxOut.txIx
            |]
          else
            [vectorStatement|
              SELECT
                applyTx.createTxId :: bytea,
                applyTx.createTxIx :: smallint,
                payoutTxOut.txId :: bytea,
                payoutTxOut.txIx :: smallint,
                payoutTxOut.rolesCurrency :: bytea,
                payoutTxOut.role :: bytea
              FROM marlowe.payoutTxOut
              NATURAL JOIN marlowe.applyTx
              ORDER BY applyTx.slotNo, payoutTxOut.txId, payoutTxOut.txIx
            |]
  pure do
    let contractIdsFiltered
          | Set.null contractIds = allPayouts
          | otherwise = filter (flip Set.member contractIds . contractId) allPayouts
    let filtered
          | Set.null roleTokens = contractIdsFiltered
          | otherwise = filter (flip Set.member roleTokens . payoutRefRoleToken) contractIdsFiltered
    let ordered = case rangeDirection of
          Ascending -> filtered
          Descending -> reverse filtered
    delimited <- case rangeStart of
      Nothing -> pure ordered
      Just startFrom -> do
        guard $ any ((== startFrom) . payout) ordered
        pure $ dropWhile ((/= startFrom) . payout) ordered
    let items = take rangeLimit . drop rangeOffset $ delimited
    pure
      Page
        { items
        , nextRange = do
            PayoutRef{..} <- listToMaybe $ reverse items
            pure $ Range{rangeStart = Just payout, rangeOffset = 1, ..}
        , totalCount = length filtered
        }

payoutRefRoleToken :: PayoutRef -> AssetId
payoutRefRoleToken PayoutRef{..} = AssetId rolesCurrency role

uncurry6 :: (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 g (a, b, c, d, e, f) = g a b c d e f
