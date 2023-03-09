{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders
  where

import Control.Foldl (Fold)
import qualified Control.Foldl as Fold
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16, Int64)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Hasql.TH (foldStatement, maybeStatement, singletonStatement)
import qualified Hasql.Transaction as T
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api
  ( Address(..)
  , BlockHeader(..)
  , BlockHeaderHash(..)
  , Credential(..)
  , PolicyId(..)
  , ScriptHash(..)
  , TxId(..)
  , TxOutRef(..)
  , paymentCredential
  )
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweMetadataTag(getMarloweMetadataTag)
  , MarloweVersion(MarloweV1)
  , SomeMarloweVersion(SomeMarloweVersion)
  , emptyMarloweTransactionMetadata
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Prelude hiding (init)

getHeaders
  :: ContractFilter
  -> Range ContractId
  -> T.Transaction (Maybe (Page ContractId ContractHeader))
getHeaders _ Range{..}
  -- Invalid requests. Note rangeLimit == 0 is invalid because it produces no
  -- results and causes infinite paging, which is potentially dangerous for
  -- clients as they could get caught in an infinite loop if consuming all
  -- pages.
  | rangeLimit <= 0 || rangeOffset < 0 = pure Nothing

getHeaders cFilter@ContractFilter{..} Range{rangeStart = Just (ContractId TxOutRef{..}), ..} = runMaybeT do
  pivot <- MaybeT case (Set.null tags, Set.null roleCurrencies) of
    -- unconstrained
    (True, True) -> T.statement (unTxId txId, fromIntegral txIx)
      [maybeStatement|
        SELECT slotNo :: bigint, txId :: bytea, txIx :: smallint
        FROM marlowe.createTxOut
        WHERE txId = $1 :: bytea
          AND txIx = $2 :: smallint
      |]

    -- role currencies constrained
    (True, False) -> T.statement (unTxId txId, fromIntegral txIx, V.fromList $ unPolicyId <$> Set.toList roleCurrencies)
      [maybeStatement|
        SELECT createTxOut.slotNo :: bigint, createTxOut.txId :: bytea, createTxOut.txIx :: smallint
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE createTxOut.txId = $1 :: bytea
          AND createTxOut.txIx = $2 :: smallint
      |]

    -- tags constrained
    (False, True) -> T.statement (unTxId txId, fromIntegral txIx, V.fromList $ getMarloweMetadataTag <$> Set.toList tags)
      [maybeStatement|
        SELECT createTxOut.slotNo :: bigint, createTxOut.txId :: bytea, createTxOut.txIx :: smallint
        FROM marlowe.createTxOut
        WHERE createTxOut.txId = $1 :: bytea
          AND createTxOut.txIx = $2 :: smallint
          AND EXISTS
            ( SELECT 1
              FROM marlowe.contractTxOutTag
              JOIN (SELECT UNNEST($3 :: text[]) AS tag) as tags USING (tag)
              WHERE contractTxOutTag.txId = createTxOut.txId
                AND contractTxOutTag.txIx = createTxOut.txIx
            )
      |]

    -- tags and role currencies constrained
    (False, False) -> T.statement
      ( unTxId txId
      , fromIntegral txIx
      , V.fromList $ unPolicyId <$> Set.toList roleCurrencies
      , V.fromList $ getMarloweMetadataTag <$> Set.toList tags
      )
      [maybeStatement|
        SELECT createTxOut.slotNo :: bigint, createTxOut.txId :: bytea, createTxOut.txIx :: smallint
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE createTxOut.txId = $1 :: bytea
          AND createTxOut.txIx = $2 :: smallint
          AND EXISTS
            ( SELECT 1
              FROM marlowe.contractTxOutTag
              JOIN (SELECT UNNEST($4 :: text[]) AS tag) as tags USING (tag)
              WHERE contractTxOutTag.txId = createTxOut.txId
                AND contractTxOutTag.txIx = createTxOut.txIx
            )
      |]
  totalCount <- lift $ getTotalCount cFilter
  lift $ getHeadersFrom cFilter totalCount pivot rangeOffset rangeLimit rangeDirection

-- TODO If we ever add another filter field, the combinations of cases will get
-- even worse. At that point, we should consider using a different library to
-- generate the sql queries dynamically, as they are mostly the same with
-- different JOINs and conditions.
getHeaders cFilter@ContractFilter{..} Range{..} = do
  totalCount <- getTotalCount cFilter
  Just <$> case (Set.null tags, Set.null roleCurrencies, rangeDirection) of
    (True, True, Descending) -> T.statement nullFilterParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (True, True, Ascending) -> T.statement nullFilterParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (True, False, Descending) -> T.statement nullTagsParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (True, False, Ascending) -> T.statement nullTagsParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (False, True, Descending) -> T.statement nullRoleCurrenciesParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        WHERE EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($3 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
        ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (False, True, Ascending) -> T.statement nullRoleCurrenciesParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        WHERE EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($3 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
        ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (False, False, Descending) -> T.statement nonNullFilterParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($4 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
        ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)

    (False, False, Ascending) -> T.statement nonNullFilterParams $
      [foldStatement|
        SELECT
          createTxOut.slotNo :: bigint,
          createTxOut.blockId :: bytea,
          createTxOut.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN (SELECT UNNEST($3 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
        WHERE EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($4 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
        ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage decodeContractId decodeContractHeader rangeLimit rangeDirection totalCount)
  where
    -- Load one extra item so we can detect when we've hit the end
    nullFilterParams = (fromIntegral rangeOffset, fromIntegral rangeLimit + 1)
    nullTagsParams = (fromIntegral rangeOffset, fromIntegral rangeLimit + 1, V.fromList $ unPolicyId <$> Set.toList roleCurrencies)
    nullRoleCurrenciesParams = (fromIntegral rangeOffset, fromIntegral rangeLimit + 1, V.fromList $ getMarloweMetadataTag <$> Set.toList tags)
    nonNullFilterParams =
      ( fromIntegral rangeOffset
      , fromIntegral rangeLimit + 1
      , V.fromList $ unPolicyId <$> Set.toList roleCurrencies
      , V.fromList $ getMarloweMetadataTag <$> Set.toList tags
      )

getHeadersFrom
  :: ContractFilter
  -> Int
  -> (Int64, ByteString, Int16)
  -> Int
  -> Int
  -> Order
  -> T.Transaction (Page ContractId ContractHeader)
getHeadersFrom ContractFilter{..} totalCount (pivotSlot, pivotTxId, pivotTxIx) offset limit order = case (Set.null tags, Set.null roleCurrencies, order) of
  (True, True, Descending) -> T.statement nullFilterParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      WHERE createTxOut.slotNo < $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId < $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx <= $3 :: smallint
            )
          )
        )
      ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Descending totalCount)

  (True, True, Ascending) -> T.statement nullFilterParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      WHERE createTxOut.slotNo > $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId > $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx >= $3 :: smallint
            )
          )
        )
      ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Ascending totalCount)

  (True, False, Descending) -> T.statement nullTagsParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      JOIN (SELECT UNNEST($6 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
      WHERE createTxOut.slotNo < $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId < $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx <= $3 :: smallint
            )
          )
        )
      ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Descending totalCount)

  (True, False, Ascending) -> T.statement nullTagsParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      JOIN (SELECT UNNEST($6 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
      WHERE createTxOut.slotNo > $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId > $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx >= $3 :: smallint
            )
          )
        )
      ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Ascending totalCount)

  (False, True, Descending) -> T.statement nullRoleCurrenciesParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      WHERE (createTxOut.slotNo < $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId < $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx <= $3 :: smallint
            )
          )
        ))
        AND EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($6 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
      ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Descending totalCount)

  (False, True, Ascending) -> T.statement nullRoleCurrenciesParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      WHERE (createTxOut.slotNo > $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId > $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx >= $3 :: smallint
            )
          )
        ))
        AND EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($6 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
      ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Ascending totalCount)

  (False, False, Descending) -> T.statement nonNullFilterParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      JOIN (SELECT UNNEST($6 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
      WHERE (createTxOut.slotNo < $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId < $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx <= $3 :: smallint
            )
          )
        ))
        AND EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($7 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
      ORDER BY createTxOut.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Descending totalCount)

  (False, False, Ascending) -> T.statement nonNullFilterParams $
    [foldStatement|
      SELECT
        createTxOut.slotNo :: bigint,
        createTxOut.blockId :: bytea,
        createTxOut.blockNo :: bigint,
        createTxOut.txId :: bytea,
        createTxOut.txIx :: smallint,
        contractTxOut.rolesCurrency :: bytea,
        createTxOut.metadata :: bytea?,
        txOut.address :: bytea,
        contractTxOut.payoutScriptHash :: bytea
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN marlowe.txOut USING (txId, txIx)
      JOIN (SELECT UNNEST($6 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
      WHERE (createTxOut.slotNo > $1 :: bigint OR
        ( createTxOut.slotNo = $1 :: bigint AND
          ( createTxOut.txId > $2 :: bytea OR
            ( createTxOut.txId = $2 :: bytea AND
                createTxOut.txIx >= $3 :: smallint
            )
          )
        ))
        AND EXISTS
          ( SELECT 1
            FROM marlowe.contractTxOutTag
            JOIN (SELECT UNNEST($7 :: text[]) AS tag) as tags USING (tag)
            WHERE contractTxOutTag.txId = createTxOut.txId
              AND contractTxOutTag.txIx = createTxOut.txIx
          )
      ORDER BY createTxOut.slotNo, createTxOut.txId, createTxOut.txIx
      OFFSET ($4 :: int) ROWS
      FETCH NEXT ($5 :: int) ROWS ONLY
    |] (foldPage decodeContractId decodeContractHeader limit Ascending totalCount)
  where
    -- Load one extra item so we can detect when we've hit the end
    nullFilterParams = (pivotSlot, pivotTxId, pivotTxIx, fromIntegral offset, fromIntegral limit + 1)
    nullTagsParams =
      ( pivotSlot
      , pivotTxId
      , pivotTxIx
      , fromIntegral offset
      , fromIntegral limit + 1
      , V.fromList $ unPolicyId <$> Set.toList roleCurrencies
      )
    nullRoleCurrenciesParams =
      ( pivotSlot
      , pivotTxId
      , pivotTxIx
      , fromIntegral offset
      , fromIntegral limit + 1
      , V.fromList $ getMarloweMetadataTag <$> Set.toList tags
      )
    nonNullFilterParams =
      ( pivotSlot
      , pivotTxId
      , pivotTxIx
      , fromIntegral offset
      , fromIntegral limit + 1
      , V.fromList $ unPolicyId <$> Set.toList roleCurrencies
      , V.fromList $ getMarloweMetadataTag <$> Set.toList tags
      )

getTotalCount :: ContractFilter -> T.Transaction Int
getTotalCount ContractFilter{..} = fromIntegral <$> case (Set.null tags, Set.null roleCurrencies) of
  (True, True) -> T.statement ()
    [singletonStatement|
      SELECT COUNT(*) :: int FROM marlowe.createTxOut
    |]

  (True, False) -> T.statement (V.fromList $ unPolicyId <$> Set.toList roleCurrencies)
    [singletonStatement|
      SELECT COUNT(*) :: int
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN (SELECT UNNEST($1 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
    |]

  (False, True) -> T.statement (V.fromList $ getMarloweMetadataTag <$> Set.toList tags)
    [singletonStatement|
      SELECT COUNT(*) :: int
      FROM marlowe.createTxOut
      WHERE EXISTS
        ( SELECT 1
          FROM marlowe.contractTxOutTag
          JOIN (SELECT UNNEST($1 :: text[]) AS tag) as tags USING (tag)
          WHERE contractTxOutTag.txId = createTxOut.txId
            AND contractTxOutTag.txIx = createTxOut.txIx
        )
    |]

  (False, False) -> T.statement
    ( V.fromList $ unPolicyId <$> Set.toList roleCurrencies
    , V.fromList $ getMarloweMetadataTag <$> Set.toList tags
    )
    [singletonStatement|
      SELECT COUNT(*) :: int
      FROM marlowe.createTxOut
      JOIN marlowe.contractTxOut USING (txId, txIx)
      JOIN (SELECT UNNEST($1 :: bytea[]) AS rolesCurrency) as roles USING (rolesCurrency)
      WHERE EXISTS
        ( SELECT 1
          FROM marlowe.contractTxOutTag
          JOIN (SELECT UNNEST($2 :: text[]) AS tag) as tags USING (tag)
          WHERE contractTxOutTag.txId = createTxOut.txId
            AND contractTxOutTag.txIx = createTxOut.txIx
        )
    |]

type ResultRow =
  ( Int64
  , ByteString
  , Int64
  , ByteString
  , Int16
  , ByteString
  , Maybe ByteString
  , ByteString
  , ByteString
  )

foldPage :: (row -> id) -> (row -> item) -> Int -> Order -> Int -> Fold row (Page id item)
foldPage decodeId decodeItem limit direction totalCount = Page
  <$> foldItems decodeItem limit
  <*> foldNextRange decodeId limit direction
  <*> pure totalCount

foldItems :: (row -> item) -> Int -> Fold row [item]
foldItems decodeItem limit = fmap decodeItem . take limit <$> Fold.list

decodeContractHeader :: ResultRow -> ContractHeader
decodeContractHeader
  ( slotNo
  , blockHeaderHash
  , blockNo
  , txId
  , txIx
  , rolesCurrency
  , metadata
  , address
  , payoutScriptHash
  ) = ContractHeader
    { contractId = ContractId (TxOutRef (TxId txId) (fromIntegral txIx))
    , rolesCurrency = PolicyId rolesCurrency
    , metadata = maybe emptyMarloweTransactionMetadata (runGet get . fromStrict) metadata
    , marloweScriptHash = fromJust do
        credential <- paymentCredential $ Address address
        case credential of
          ScriptCredential hash -> pure hash
          _ -> Nothing
    , marloweScriptAddress = Address address
    , payoutScriptHash = ScriptHash payoutScriptHash
    , marloweVersion = SomeMarloweVersion MarloweV1
    , blockHeader = BlockHeader
        (fromIntegral slotNo)
        (BlockHeaderHash blockHeaderHash)
        (fromIntegral blockNo)
    }

foldNextRange :: (row -> id) -> Int -> Order -> Fold row (Maybe (Range id))
foldNextRange decodeId rangeLimit rangeDirection = do
  mNext <- Fold.index rangeLimit
  pure do
    row <- mNext
    pure Range
      { rangeStart = Just $ decodeId row
      , rangeOffset = 0
      , ..
      }

decodeContractId :: ResultRow -> ContractId
decodeContractId (_, _, _, txId, txIx, _, _, _, _) = ContractId (TxOutRef (TxId txId) (fromIntegral txIx))
