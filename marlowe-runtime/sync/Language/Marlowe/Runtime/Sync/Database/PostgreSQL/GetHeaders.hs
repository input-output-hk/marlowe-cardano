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
  (ContractId(..), MarloweVersion(MarloweV1), SomeMarloweVersion(SomeMarloweVersion))
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..))
import Prelude hiding (init)

getHeaders
  :: Range ContractId
  -> T.Transaction (Maybe (Page ContractId ContractHeader))
getHeaders Range{..}
  -- Invalid requests. Note rangeLimit == 0 is invalid because it produces no
  -- results and causes infinite paging, which is potentially dangerous for
  -- clients as they could get caught in an infinite loop if consuming all
  -- pages.
  | rangeLimit <= 0 || rangeOffset < 0 = pure Nothing

getHeaders Range{rangeStart = Just (ContractId TxOutRef{..}), ..} = runMaybeT do
  let params = (unTxId txId, fromIntegral txIx)
  pivot <- MaybeT $ T.statement params
    [maybeStatement|
      SELECT slotNo :: bigint, txId :: bytea, txIx :: smallint
      FROM marlowe.createTxOut
      WHERE txId = $1 :: bytea
        AND txIx = $2 :: smallint
    |]
  totalCount <- lift $ fromIntegral <$> T.statement () [singletonStatement| SELECT COUNT(*) :: int FROM marlowe.createTxOut |]
  lift $ getHeadersFrom totalCount pivot rangeOffset rangeLimit rangeDirection

getHeaders Range{..} = do
  totalCount <- fromIntegral <$> T.statement () [singletonStatement| SELECT COUNT(*) :: int FROM marlowe.createTxOut |]
  Just <$> T.statement params case rangeDirection of
    Descending ->
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
      |] (foldPage rangeLimit rangeDirection totalCount)

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
      |] (foldPage rangeLimit rangeDirection totalCount)
  where
    -- Load one extra item so we can detect when we've hit the end
    params = (fromIntegral rangeOffset, fromIntegral rangeLimit + 1)

getHeadersFrom
  :: Int
  -> (Int64, ByteString, Int16)
  -> Int
  -> Int
  -> Order
  -> T.Transaction (Page ContractId ContractHeader)
getHeadersFrom totalCount (pivotSlot, pivotTxId, pivotTxIx) offset limit = T.statement params . \case
  Descending ->
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
    |] (foldPage limit Descending totalCount)

  Ascending ->
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
    |] (foldPage limit Ascending totalCount)
  where
    -- Load one extra item so we can detect when we've hit the end
    params = (pivotSlot, pivotTxId, pivotTxIx, fromIntegral offset, fromIntegral limit + 1)

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

foldPage :: Int -> Order -> Int -> Fold ResultRow (Page ContractId ContractHeader)
foldPage limit direction totalCount = Page
  <$> foldItems limit
  <*> foldNextRange limit direction
  <*> pure totalCount

foldItems :: Int -> Fold ResultRow [ContractHeader]
foldItems limit = fmap decodeContractHeader . take limit <$> Fold.list

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

foldNextRange :: Int -> Order -> Fold ResultRow (Maybe (Range ContractId))
foldNextRange rangeLimit rangeDirection = do
  mNext <- Fold.index rangeLimit
  pure do
    (_, _, _, txId, txIx, _, _, _, _) <- mNext
    pure Range
      { rangeStart = Just $ ContractId (TxOutRef (TxId txId) (fromIntegral txIx))
      , rangeOffset = 0
      , ..
      }
