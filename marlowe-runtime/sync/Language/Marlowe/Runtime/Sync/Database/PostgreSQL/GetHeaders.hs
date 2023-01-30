{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.Sync.Database.PostgreSQL.GetHeaders
  where

import Control.Foldl (Fold)
import qualified Control.Foldl as Fold
import Data.Binary (get)
import Data.Binary.Get (runGet)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromStrict)
import Data.Int (Int16, Int64)
import Data.Maybe (fromJust)
import Hasql.TH (foldStatement, singletonStatement)
import qualified Hasql.Transaction as T
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
  (ContractId(..), MarloweVersion(MarloweV1), SomeMarloweVersion(SomeMarloweVersion))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Language.Marlowe.Runtime.Sync.Database (Order(..), Page(..), Range(..))
import Prelude hiding (init)

getHeaders
  :: Range ContractId
  -> T.Transaction (Page ContractId ContractHeader)
getHeaders range@Range{rangeStart = Just (ContractId TxOutRef{..}), ..} = do
  totalItems <- fromIntegral <$> T.statement () [singletonStatement| SELECT COUNT(*) :: int FROM marlowe.createTxOut |]
  T.statement params case rangeDirection of
    Descending ->
      [foldStatement|
        SELECT
          block.slotNo :: bigint,
          block.id :: bytea,
          block.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN marlowe.block
          ON block.id = createTxOut.blockId
        JOIN
          ( SELECT slotNo, txId, txIx
            FROM marlowe.createTxOut
            JOIN marlowe.block
              ON block.id = createTxOut.blockId
            WHERE txId = $1 :: bytea
              AND txIx = $2 :: smallint
          ) AS pivot
          ON block.slotNo < pivot.slotNo OR
            ( block.slotNo = pivot.slotNo AND
              ( createTxOut.txId < pivot.txId OR
                ( createTxOut.txId = pivot.txId AND
                    createTxOut.txIx <= pivot.txIx
                )
              )
            )
        ORDER BY block.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
        OFFSET ($3 :: int) ROWS
        FETCH NEXT ($4 :: int) ROWS ONLY
      |] (foldPage range totalItems)

    Ascending ->
      [foldStatement|
        SELECT
          block.slotNo :: bigint,
          block.id :: bytea,
          block.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN marlowe.block
          ON block.id = createTxOut.blockId
        JOIN
          ( SELECT slotNo, txId, txIx
            FROM marlowe.createTxOut
            JOIN marlowe.block
              ON block.id = createTxOut.blockId
            WHERE txId = $1 :: bytea
              AND txIx = $2 :: smallint
          ) AS pivot
          ON block.slotNo > pivot.slotNo OR
            ( block.slotNo = pivot.slotNo AND
              ( createTxOut.txId > pivot.txId OR
                ( createTxOut.txId = pivot.txId AND
                    createTxOut.txIx >= pivot.txIx
                )
              )
            )
        ORDER BY block.slotNo, createTxOut.txId, createTxOut.txIx
        OFFSET ($3 :: int) ROWS
        FETCH NEXT ($4 :: int) ROWS ONLY
      |] (foldPage range totalItems)
  where
    params = (unTxId txId, fromIntegral txIx, fromIntegral rangeOffset, fromIntegral rangeLimit)

getHeaders range@Range{..} = do
  totalItems <- fromIntegral <$> T.statement () [singletonStatement| SELECT COUNT(*) :: int FROM marlowe.createTxOut |]
  T.statement params case rangeDirection of
    Descending ->
      [foldStatement|
        SELECT
          block.slotNo :: bigint,
          block.id :: bytea,
          block.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN marlowe.block
          ON block.id = createTxOut.blockId
        ORDER BY block.slotNo DESC, createTxOut.txId DESC, createTxOut.txIx DESC
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage range totalItems)

    Ascending ->
      [foldStatement|
        SELECT
          block.slotNo :: bigint,
          block.id :: bytea,
          block.blockNo :: bigint,
          createTxOut.txId :: bytea,
          createTxOut.txIx :: smallint,
          contractTxOut.rolesCurrency :: bytea,
          createTxOut.metadata :: bytea?,
          txOut.address :: bytea,
          contractTxOut.payoutScriptHash :: bytea
        FROM marlowe.createTxOut
        JOIN marlowe.contractTxOut USING (txId, txIx)
        JOIN marlowe.txOut USING (txId, txIx)
        JOIN marlowe.block
          ON block.id = createTxOut.blockId
        ORDER BY block.slotNo, createTxOut.txId, createTxOut.txIx
        OFFSET ($1 :: int) ROWS
        FETCH NEXT ($2 :: int) ROWS ONLY
      |] (foldPage range totalItems)
  where
    params = (fromIntegral rangeOffset, fromIntegral rangeLimit)


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

foldPage :: Range ContractId -> Int -> Fold ResultRow (Page ContractId ContractHeader)
foldPage range totalItems = Page
  <$> foldItems
  <*> foldNextRange range
  <*> pure totalItems

foldItems :: Fold ResultRow [ContractHeader]
foldItems = fmap decodeContractHeader <$> Fold.list

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
    , metadata = maybe mempty (runGet get. fromStrict) metadata
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

foldNextRange :: Range ContractId -> Fold ResultRow (Maybe (Range ContractId))
foldNextRange range = fmap (decodeNextRange range) <$> Fold.last

decodeNextRange :: Range ContractId -> ResultRow -> Range ContractId
decodeNextRange Range{..} (_, _, _, txId, txIx, _, _, _, _) = Range
  { rangeStart = Just $ ContractId (TxOutRef (TxId txId) (fromIntegral txIx))
  , rangeOffset = rangeOffset + 1
  , ..
  }
