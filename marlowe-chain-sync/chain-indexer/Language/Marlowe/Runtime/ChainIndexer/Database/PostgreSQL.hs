{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

{- IMPORTANT PERFORMANCE WARNING - non-obvious requirement for filtering on addresses

   If you want to write a query that filters on chain.txOut.address, be aware
   that the database does not index the address column directly (because of some
   Byron addresses that are prohibitively large). Instead, it indexes an MD5 hash
   of the address.

   This means that if you want your query to be at all performant, it is not
   enough to say, for example

    WHERE txOut.address = myAddress

   you have to do something like this:

    WHERE txOut.address = myAddress AND CAST(MD5(txOut.address) as uuid) = CAST(MD5(myAddress) as uuid)

   On mainnet, this can make as much as a 600x difference in performance!
-}

module Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL
  ( databaseQueries
  ) where

import Cardano.Api
  ( AddressAny
  , AsType(..)
  , AssetId(..)
  , AssetName(..)
  , Block(..)
  , BlockHeader(..)
  , BlockInMode(..)
  , BlockNo(..)
  , CardanoMode
  , ChainPoint(..)
  , CtxTx
  , EraInMode
  , IsCardanoEra
  , Lovelace(..)
  , PolicyId
  , Quantity(Quantity)
  , ScriptValidity(..)
  , SerialiseAsCBOR(serialiseToCBOR)
  , SerialiseAsRawBytes(..)
  , ShelleyBasedEra(ShelleyBasedEraAlonzo, ShelleyBasedEraBabbage)
  , SlotNo(..)
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxId
  , TxIn(..)
  , TxInsCollateral(..)
  , TxIx(..)
  , TxMetadataInEra(..)
  , TxMintValue(..)
  , TxOut(..)
  , TxOutValue(..)
  , TxReturnCollateral(..)
  , TxScriptValidity(..)
  , TxValidityLowerBound(..)
  , TxValidityUpperBound(..)
  , getTxBody
  , getTxId
  , selectLovelace
  , valueToList
  )
import Cardano.Api.Shelley (Hash(..), Tx(..), toShelleyTxIn)
import Cardano.Binary (serialize')
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWitness as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.SafeHash (originalBytes)
import Cardano.Ledger.Shelley.API.Types (StrictMaybe(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Short (fromShort, toShort)
import Data.Foldable (toList)
import Data.Int (Int16, Int64)
import qualified Data.Map.Strict as M
import Data.Profunctor (rmap)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.Session (Session)
import Hasql.Statement (refineResult)
import Hasql.TH (resultlessStatement, vectorStatement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as TS
import Language.Marlowe.Runtime.ChainIndexer.Database
  ( CardanoBlock
  , CommitBlocks(..)
  , CommitGenesisBlock(..)
  , CommitRollback(..)
  , DatabaseQueries(DatabaseQueries)
  , GetGenesisBlock(..)
  , GetIntersectionPoints(..)
  , hoistCommitBlocks
  , hoistCommitGenesisBlock
  , hoistCommitRollback
  , hoistGetGenesisBlock
  , hoistGetIntersectionPoints
  )
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock(..), GenesisTx(..))
import Ouroboros.Network.Point (WithOrigin(..))
import Prelude hiding (init)

-- | PostgreSQL implementation for the chain sync database queries.
databaseQueries :: GenesisBlock -> DatabaseQueries Session
databaseQueries genesisBlock = DatabaseQueries
  (hoistCommitRollback (TS.transaction TS.ReadCommitted TS.Write) $ commitRollback genesisBlock)
  (hoistCommitBlocks (TS.transaction TS.ReadCommitted TS.Write) commitBlocks)
  (hoistCommitGenesisBlock (TS.transaction TS.ReadCommitted TS.Write) commitGenesisBlock)
  (hoistGetIntersectionPoints (TS.transaction TS.ReadCommitted TS.Read) getIntersectionPoints)
  (hoistGetGenesisBlock (TS.transaction TS.ReadCommitted TS.Read) getGenesisBlock)


-- GetGenesisBlock

getGenesisBlock :: GetGenesisBlock Transaction
getGenesisBlock = GetGenesisBlock $ HT.statement () $ refineResult (decodeResults . V.toList)
  [vectorStatement|
    SELECT block.id :: bytea
         , tx.id :: bytea
         , txOut.lovelace :: bigint
         , txOut.address :: bytea
    FROM   chain.block AS block
    JOIN   chain.tx    AS tx    ON tx.blockId = block.id
                               AND tx.slotNo = block.slotNo
    JOIN   chain.txOut AS txOut ON txOut.txId   = tx.id
    WHERE  block.slotNo = -1
  |]
  where
    decodeResults :: [(ByteString, ByteString, Int64, ByteString)] -> Either Text (Maybe GenesisBlock)
    decodeResults [] = Right Nothing
    decodeResults rows@((hash, _, _, _) : _) =
      Just . GenesisBlock (HeaderHash $ toShort hash) . Set.fromList <$> traverse decodeGenesisTx rows

    decodeGenesisTx :: (ByteString, ByteString, Int64, ByteString) -> Either Text GenesisTx
    decodeGenesisTx (_, txId, lovelace, address) = GenesisTx
      <$> decodeTxId txId
      <*> pure (fromIntegral lovelace)
      <*> decodeAddressAny address

decodeTxId :: ByteString -> Either Text TxId
decodeTxId txId = case deserialiseFromRawBytes AsTxId txId of
  Nothing    -> Left $ "Invalid TxId bytes: " <> encodeBase16 txId
  Just txId' -> Right txId'

decodeAddressAny :: ByteString -> Either Text AddressAny
decodeAddressAny address = case deserialiseFromRawBytes AsAddressAny address of
  Nothing       -> Left $ "Invalid address bytes: " <> encodeBase16 address
  Just address' -> Right address'

-- GetIntersectionPoints

getIntersectionPoints :: GetIntersectionPoints Transaction
getIntersectionPoints = GetIntersectionPoints $ HT.statement () $ rmap decodeResults
  [vectorStatement|
    SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
    FROM   chain.block
    WHERE  rollbackToBlock IS NULL
    ORDER BY slotNo DESC LIMIT 2160
  |]
  where
    decodeResults :: Vector (Int64, ByteString, Int64) -> [WithOrigin BlockHeader]
    decodeResults = fmap decodeResult . V.toList

    decodeResult :: (Int64, ByteString, Int64) -> WithOrigin BlockHeader
    decodeResult (slotNo, hash, blockNo)
      | slotNo < 0 = Origin
      | otherwise = At $ BlockHeader
          (SlotNo $ fromIntegral slotNo)
          (HeaderHash $ toShort hash)
          (BlockNo $ fromIntegral blockNo)

-- CommitRollback

commitRollback :: GenesisBlock -> CommitRollback Transaction
commitRollback GenesisBlock{..} = CommitRollback \point -> do
  let
    slotNo = case point of
      ChainPointAtGenesis -> -1
      ChainPoint s _ -> slotNoToParam s
    hash = case point of
      ChainPointAtGenesis -> genesisBlockHash
      ChainPoint _ h -> h
  HT.statement (slotNo, headerHashToParam hash)
    [resultlessStatement|
      WITH blockUpdates AS
        ( UPDATE chain.block
             SET rollbackToSlot  = $1 :: bigint
               , rollbackToBlock = $2 :: bytea
           WHERE slotNo > $1 :: bigint
        )
      , deleteTxs AS
        ( DELETE
            FROM chain.tx
           WHERE slotNo > $1 :: bigint
        )
      , deleteTxOuts AS
        ( DELETE
            FROM chain.txOut
           WHERE slotNo > $1 :: bigint
        )
      , deleteTxIns AS
        ( DELETE
            FROM chain.txIn
           WHERE slotNo > $1 :: bigint
        )
      , deleteAssetOuts AS
        ( DELETE
            FROM chain.assetOut
           WHERE slotNo > $1 :: bigint
        )
      DELETE
        FROM chain.assetMint
       WHERE slotNo > $1 :: bigint
    |]

-- CommitGenesisBlock

commitGenesisBlock :: CommitGenesisBlock Transaction
commitGenesisBlock = CommitGenesisBlock \GenesisBlock{..} ->
  let
    genesisBlockTxsList = Set.toList genesisBlockTxs
    params =
      ( headerHashToParam genesisBlockHash
      , V.fromList $ serialiseToRawBytes . genesisTxId <$> genesisBlockTxsList
      , V.fromList $ serialiseToRawBytes . genesisTxAddress <$> genesisBlockTxsList
      , V.fromList $ lovelaceToParam . genesisTxLovelace <$> genesisBlockTxsList
      , V.fromList $ BS.take 2 . serialiseToRawBytes . genesisTxAddress <$> genesisBlockTxsList
      )
  in
    HT.statement params
      [resultlessStatement|
        WITH newBlock AS
          ( INSERT INTO chain.block (id, slotNo, blockNo) VALUES ($1 :: bytea, -1, -1)
            RETURNING id
          )
        , newTxs AS
          ( INSERT INTO chain.tx (id, blockId, slotNo, isValid)
            SELECT tx.*, block.id, -1, true
            FROM   (SELECT * FROM UNNEST ($2 :: bytea[])) AS tx
            JOIN   newBlock AS block ON true
          )
        INSERT INTO chain.txOut (txId, address, lovelace, addressHeader, slotNo, txIx, isCollateral)
        SELECT *, -1, 0, false FROM UNNEST ($2 :: bytea[], $3 :: bytea[], $4 :: bigint[], $5 :: bytea[])
      |]

-- CommitBlocks

data BlockRow = BlockRow
  { hash    :: !ByteString
  , slotNo  :: !Int64
  , blockNo :: !Int64
  }

data TxRow = TxRow
  { blockHash          :: !ByteString
  , txId               :: !ByteString
  , slotNo             :: !Int64
  , validityLowerBound :: !(Maybe Int64)
  , validityUpperBound :: !(Maybe Int64)
  , metadata    :: !(Maybe ByteString)
  , isValid            :: !Bool
  }

data TxOutRow = TxOutRow
  { txId         :: !ByteString
  , txIx         :: !Int16
  , slotNo       :: !Int64
  , address      :: !ByteString
  , lovelace     :: !Int64
  , datumHash    :: !(Maybe ByteString)
  , datumBytes   :: !(Maybe ByteString)
  , isCollateral :: !Bool
  }

data TxInRow = TxInRow
  { txOutId            :: !ByteString
  , txOutIx            :: !Int16
  , txInId             :: !ByteString
  , slotNo             :: !Int64
  , redeemerDatumBytes :: !(Maybe ByteString)
  , isCollateral       :: !Bool
  }

data AssetOutRow = AssetOutRow
  { policyId :: !ByteString
  , name     :: !ByteString
  , txId     :: !ByteString
  , txIx     :: !Int16
  , slotNo   :: !Int64
  , quantity :: !Int64
  }

data AssetMintRow = AssetMintRow
  { policyId :: !ByteString
  , name     :: !ByteString
  , txId     :: !ByteString
  , slotNo   :: !Int64
  , quantity :: !Int64
  }

data SomeTx = forall era. IsCardanoEra era =>
  SomeTx (Hash BlockHeader) SlotNo (Tx era) (EraInMode era CardanoMode)

data SomeTxOut = forall era. IsCardanoEra era =>
  SomeTxOut TxId TxIx SlotNo (TxOut CtxTx era) (Maybe ByteString, Maybe ByteString) Bool (EraInMode era CardanoMode)

data SomeTxIn = SomeTxIn TxId TxIx TxId SlotNo (Maybe ByteString) Bool

data Asset = Asset PolicyId AssetName deriving (Eq, Ord)

data AssetMint = AssetMint TxId SlotNo PolicyId AssetName Quantity

data AssetOut = AssetOut TxId TxIx SlotNo PolicyId AssetName Quantity

-- TODO use COPY ... FROM when catching up for faster bulk inserts.
-- See https://hackage.haskell.org/package/postgresql-libpq-0.9.4.3/docs/Database-PostgreSQL-LibPQ.html#g:8
-- For how to do this with postgresql-libpg. You can expose the underlying
-- libpg connection from a `Session` using `withLibPQConnection`
-- (see https://hackage.haskell.org/package/hasql-1.6.0.1/docs/Hasql-Connection.html#v:withLibPQConnection)
--
-- NOTE: If this query ever gets refactored, it is extremely important that all
-- writes are atomic at the block level (i.e. it should be impossible for a
-- block to be partially saved). This is because the server may be shut down at
-- any moment, and the connection to the database will be severed.
--
-- NOTE: Mainnet epochs 0 through 175 contain epoch-boundary blocks (EBBs) between
-- the last block of one epoch and the first block of the new epoch. Such blocks
-- have a hash, but never any transactions, and are recorded in this index as
-- belonging to the new epoch. Beware that if a unique key is ever added to the
-- index for block or slot number, then these blocks will violate that new
-- constraint.
commitBlocks :: CommitBlocks Transaction
commitBlocks = CommitBlocks \blocks ->
  let
    txs = extractTxs =<< blocks
    txOuts = extractTxOuts =<< txs
    txIns = extractTxIns =<< txs
    assetOuts = extractAssetOuts =<< txOuts
    assetMints = extractAssetMints =<< txs
   in
    HT.statement (params blocks txs txOuts txIns assetOuts assetMints)
      [resultlessStatement|
        WITH blockInputs (id, slotNo, blockNo) AS
          ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bigint[], $3 :: bigint[])
          )
        , newBlocks AS
          ( INSERT INTO chain.block (id, slotNo, blockNo)
            SELECT * FROM blockInputs AS input
            WHERE NOT EXISTS
              ( SELECT 1
                FROM   chain.block AS block
                WHERE  input.id = block.id AND input.slotNo = block.slotNo
              )
          )
        , blockUpdates AS
          ( UPDATE chain.block
               SET rollbackToBlock = NULL, rollbackToSlot = NULL
              FROM blockInputs AS input
             WHERE chain.block.id = input.id
          )
        , txInputs (id, blockId, slotNo, validityLowerBound, validityUpperBound, metadata, isValid) AS
          ( SELECT * FROM UNNEST ($4 :: bytea[], $5 :: bytea[], $6 :: bigint[], $7 :: bigint?[], $8 :: bigint?[], $9 :: bytea?[], $10 :: boolean[])
          )
        , newTxs AS
          ( INSERT INTO chain.tx (id, blockId, slotNo, validityLowerBound, validityUpperBound, metadata, isValid)
            SELECT tx.id, tx.blockId, tx.slotNo, tx.validityLowerBound, tx.validityUpperBound, tx.metadata, tx.isValid
            FROM   txInputs AS tx
          )
        , txOutInputs (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral) AS
          ( SELECT * FROM UNNEST ($11 :: bytea[], $12 :: smallint[], $13 :: bigint[], $14 :: bytea[], $15 :: bigint[], $16 :: bytea?[], $17 :: bytea?[], $18 :: boolean[])
          )
        , newTxOuts AS
          ( INSERT INTO chain.txOut (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral, addressHeader, addressPaymentCredential, addressStakeAddressReference)
            SELECT txOut.txId
                 , txOut.txIx
                 , txOut.slotNo
                 , txOut.address
                 , txOut.lovelace
                 , txOut.datumHash
                 , txOut.datumBytes
                 , txOut.isCollateral
                 , substring(txOut.address from 0 for 2)
                 , CASE
                    WHEN length(txOut.address) in (29, 57) THEN substring(txOut.address from 2 for 28)
                    ELSE NULL
                   END
                 , CASE
                    WHEN length(txOut.address) = 57 THEN substring(txOut.address from 30)
                    ELSE NULL
                   END
            FROM   txOutInputs AS txOut
          )
        , txInInputs (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral) AS
          ( SELECT * FROM UNNEST ($19 :: bytea[], $20 :: smallint[], $21 :: bytea[], $22 :: bigint[], $23 :: bytea?[], $24 :: boolean[])
          )
        , newTxIns AS
          ( INSERT INTO chain.txIn (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral)
            SELECT txIn.txOutId, txIn.txOutIx, txIn.txInId, txIn.slotNo, txIn.redeemerDatumBytes, txIn.isCollateral
            FROM   txInInputs  AS txIn
          )
        , assetOutInputs (policyId, name, txOutId, txOutIx, slotNo, quantity) AS
          ( SELECT * FROM UNNEST ($25 :: bytea[], $26 :: bytea[], $27 :: bytea[], $28 :: smallint[], $29 :: bigint[], $30 :: bigint[])
          )
        , assetMintInputs (policyId, name, txId, slotNo, quantity) AS
          ( SELECT * FROM UNNEST ($31 :: bytea[], $32 :: bytea[], $33 :: bytea[], $34 :: bigint[], $35 :: bigint[])
          )
        , newAssets AS
          ( INSERT INTO chain.asset (policyId, name)
            SELECT assetInputs.* FROM
              ( SELECT DISTINCT policyId, name FROM assetOutInputs
                UNION
                SELECT DISTINCT policyId, name FROM assetMintInputs
              ) AS assetInputs
            ON CONFLICT (policyId, name) DO NOTHING
            RETURNING id, policyId, name
          )
        , assetIds AS
          ( SELECT * FROM newAssets
            UNION
            SELECT asset.*
            FROM   assetOutInputs AS assetOut
            JOIN   chain.asset    AS asset    USING  (policyId, name)
            UNION
            SELECT asset.*
            FROM   assetMintInputs AS assetMint
            JOIN   chain.asset    AS asset    USING  (policyId, name)
          )
        , newAssetOuts AS
          ( INSERT INTO chain.assetOut (txOutId, txOutIx, slotNo, assetId, quantity)
            SELECT assetOut.txOutId, assetOut.txOutIx, assetOut.slotNo, asset.id, assetOut.quantity
            FROM   assetOutInputs AS assetOut
            JOIN   assetIds       AS asset    USING (policyId, name)
          )
        INSERT INTO chain.assetMint (txId, slotNo, assetId, quantity)
        SELECT assetMint.txId, assetMint.slotNo, asset.id, assetMint.quantity
        FROM   assetMintInputs AS assetMint
        JOIN   assetIds        AS asset     USING (policyId, name)
      |]
  where
    params blocks txs txOuts txIns assetOuts assetMints =
      ( V.fromList $ hash <$> blockRows
      , V.fromList $ (\BlockRow{..} -> slotNo) <$> blockRows
      , V.fromList $ blockNo <$> blockRows

      , V.fromList $ (\TxRow{..} -> txId) <$> txRows
      , V.fromList $ blockHash <$> txRows
      , V.fromList $ (\TxRow{..} -> slotNo) <$> txRows
      , V.fromList $ validityLowerBound <$> txRows
      , V.fromList $ validityUpperBound <$> txRows
      , V.fromList $ metadata <$> txRows
      , V.fromList $ isValid <$> txRows

      , V.fromList $ (\TxOutRow{..} -> txId) <$> txOutRows
      , V.fromList $ (\TxOutRow{..} -> txIx) <$> txOutRows
      , V.fromList $ (\TxOutRow{..} -> slotNo) <$> txOutRows
      , V.fromList $ address <$> txOutRows
      , V.fromList $ lovelace <$> txOutRows
      , V.fromList $ datumHash <$> txOutRows
      , V.fromList $ datumBytes <$> txOutRows
      , V.fromList $ (\TxOutRow{..} -> isCollateral) <$> txOutRows

      , V.fromList $ txOutId <$> txInRows
      , V.fromList $ txOutIx <$> txInRows
      , V.fromList $ txInId <$> txInRows
      , V.fromList $ (\TxInRow{..} -> slotNo) <$> txInRows
      , V.fromList $ redeemerDatumBytes <$> txInRows
      , V.fromList $ (\TxInRow{..} -> isCollateral) <$> txInRows

      , V.fromList $ (\AssetOutRow{..} -> policyId) <$> assetOutRows
      , V.fromList $ (\AssetOutRow{..} -> name) <$> assetOutRows
      , V.fromList $ (\AssetOutRow{..} -> txId) <$> assetOutRows
      , V.fromList $ (\AssetOutRow{..} -> txIx) <$> assetOutRows
      , V.fromList $ (\AssetOutRow{..} -> slotNo) <$> assetOutRows
      , V.fromList $ (\AssetOutRow{..} -> quantity) <$> assetOutRows

      , V.fromList $ (\AssetMintRow{..} -> policyId) <$> assetMintRows
      , V.fromList $ (\AssetMintRow{..} -> name) <$> assetMintRows
      , V.fromList $ (\AssetMintRow{..} -> txId) <$> assetMintRows
      , V.fromList $ (\AssetMintRow{..} -> slotNo) <$> assetMintRows
      , V.fromList $ (\AssetMintRow{..} -> quantity) <$> assetMintRows
      )
      where
        blockRows = blockRow <$> blocks
        blockRow (BlockInMode (Block (BlockHeader slotNo hash (BlockNo blockNo)) _) _)  = BlockRow
          { hash = headerHashToParam hash
          , slotNo = slotNoToParam slotNo
          , blockNo = fromIntegral blockNo
          }

        txRows = txRow <$> txs
        txRow (SomeTx blockHash slotNo tx _) = case getTxBody tx of
          body@(TxBody TxBodyContent{txValidityRange, txMetadata, txScriptValidity}) -> TxRow
            { blockHash = headerHashToParam blockHash
            , txId = serialiseToRawBytes $ getTxId body
            , slotNo = slotNoToParam slotNo
            , validityLowerBound = case fst txValidityRange of
                TxValidityNoLowerBound      -> Nothing
                TxValidityLowerBound _ slot -> Just $ slotNoToParam slot
            , validityUpperBound = case snd txValidityRange of
                TxValidityNoUpperBound _    -> Nothing
                TxValidityUpperBound _ slot -> Just $ slotNoToParam slot
            , metadata = case txMetadata of
                TxMetadataNone                          -> Nothing
                TxMetadataInEra _ metadata -> Just $ serialiseToCBOR metadata
            , isValid = case txScriptValidity of
                TxScriptValidity _ ScriptInvalid -> False
                _                                -> True
            }

        txOutRows = txOutRow <$> txOuts
        txOutRow (SomeTxOut txId txIx slotNo (TxOut address value _ _) (datumBytesHashed, datumBytes) isCollateral _) = TxOutRow
          { txId = serialiseToRawBytes txId
          , txIx = txIxToParam txIx
          , slotNo = slotNoToParam slotNo
          , address = serialiseToRawBytes address
          , lovelace = lovelaceToParam case value of
              TxOutAdaOnly _ lovelace -> lovelace
              TxOutValue _ value'     -> selectLovelace value'
          , datumHash = datumBytesHashed
          , datumBytes = datumBytes
          , isCollateral
          }

        txInRows = txInRow <$> txIns
        txInRow (SomeTxIn txOutId txOutIx txInId txInSlotNo redeemerDatumBytes isCollateral) = TxInRow
          { txOutId = serialiseToRawBytes txOutId
          , txOutIx = txIxToParam txOutIx
          , txInId = serialiseToRawBytes txInId
          , slotNo = slotNoToParam txInSlotNo
          , redeemerDatumBytes
          , isCollateral
          }

        assetOutRows = assetOutRow <$> assetOuts
        assetMintRows = assetMintRow <$> assetMints
        assetOutRow (AssetOut txId txIx slotNo policyId (AssetName name) (Quantity quantity)) = AssetOutRow
          { policyId = serialiseToRawBytes policyId
          , name
          , txId = serialiseToRawBytes txId
          , txIx = txIxToParam txIx
          , slotNo = slotNoToParam slotNo
          , quantity = fromIntegral quantity
          }
        assetMintRow (AssetMint txId slotNo policyId (AssetName name) (Quantity quantity)) = AssetMintRow
          { policyId = serialiseToRawBytes policyId
          , name
          , txId = serialiseToRawBytes txId
          , slotNo = slotNoToParam slotNo
          , quantity = fromIntegral quantity
          }

    extractTxs :: CardanoBlock -> [SomeTx]
    extractTxs (BlockInMode (Block (BlockHeader slotNo hash _) blockTxs) era) = flip (SomeTx hash slotNo) era <$> blockTxs

    extractTxOuts :: SomeTx -> [SomeTxOut]
    extractTxOuts (SomeTx _ slotNo tx era) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txScriptValidity of
        TxScriptValidity _ ScriptInvalid -> case txReturnCollateral of
          TxReturnCollateralNone     -> []
          TxReturnCollateral _ txOut -> let
                                          -- The index of the `TxOut` for collateral change on a transaction that
                                          -- fails due to an invalid script is one more than the number of `TxOut`s
                                          -- that there would have been had the transaction succeeded.
                                          index =  toEnum $ length txOuts
                                        in
                                          [SomeTxOut (getTxId body) (TxIx index) slotNo txOut (Nothing, Nothing) True era]
        _ -> do
          (ix, txOut, datumInfo) <- zip3 [0..] txOuts datums
          pure $ SomeTxOut (getTxId body) (TxIx ix) slotNo txOut datumInfo False era
      where
        datums = case tx of
          ShelleyTx ShelleyBasedEraAlonzo (Alonzo.ValidatedTx body wits _ _) ->
            let
              getDatum (Alonzo.TxOut _ _ (SJust dh)) =
                ( Just $ originalBytes dh
                , fmap serialize' . M.lookup dh . Alonzo.unTxDats $ Alonzo.txdats' wits
                )
              getDatum (Alonzo.TxOut _ _ SNothing) = (Nothing, Nothing)
            in
              toList $ getDatum <$> Alonzo.outputs' body
          ShelleyTx ShelleyBasedEraBabbage (Babbage.ValidatedTx body wits _ _) ->
            let
              getDatum (Babbage.TxOut _ _ datum _) =
                case datum of
                  Babbage.NoDatum -> (Nothing, Nothing)
                  Babbage.DatumHash dh -> ( Just $ originalBytes dh
                                          , fmap serialize' . M.lookup dh . Alonzo.unTxDats $ Babbage.txdats' wits
                                          )
                  Babbage.Datum d -> ( Just . originalBytes $ Alonzo.hashBinaryData d
                                     , Just . serialize' $ Alonzo.binaryDataToData d
                                     )
            in
              toList $ getDatum <$> Babbage.outputs' body
          _ -> (Nothing, Nothing) <$ (let TxBody TxBodyContent{txOuts} = getTxBody tx in txOuts)

    extractTxIns :: SomeTx -> [SomeTxIn]
    extractTxIns (SomeTx _ slotNo tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txScriptValidity of
        TxScriptValidity _ ScriptInvalid -> case txInsCollateral of
          TxInsCollateralNone             -> []
          TxInsCollateral _ collateralIns -> do
            TxIn txId txIx <- collateralIns
            pure $ SomeTxIn txId txIx (getTxId body) slotNo Nothing True
        _ -> do
          txIn@(TxIn txId txIx) <- fst <$> txIns
          pure $ SomeTxIn txId txIx (getTxId body) slotNo (getRedeemer txIn) False
      where
        getRedeemer :: TxIn -> Maybe ByteString
        getRedeemer = case tx of
          ShelleyTx ShelleyBasedEraAlonzo alonzoTx@Alonzo.ValidatedTx{} ->
            \txIn -> do
              (datum, _) <- Alonzo.indexedRdmrs alonzoTx $ Alonzo.Spending $ toShelleyTxIn txIn
              pure $ originalBytes $ Alonzo.dataToBinaryData datum

          ShelleyTx ShelleyBasedEraBabbage  babbageTx@Babbage.ValidatedTx{} -> do
            \txIn -> do
              (datum, _) <- Babbage.indexedRdmrs babbageTx $ Babbage.Spending $ toShelleyTxIn txIn
              pure $ originalBytes $ Alonzo.dataToBinaryData datum

          _ -> const Nothing

    extractAssetMints :: SomeTx -> [AssetMint]
    extractAssetMints (SomeTx _ slotNo tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txMintValue of
        TxMintNone -> []
        TxMintValue _ value _ -> do
          (assetId, quantity) <- valueToList value
          case assetId of
            AdaAssetId            -> []
            AssetId policyId name -> [AssetMint (getTxId body) slotNo policyId name quantity]

    extractAssetOuts :: SomeTxOut -> [AssetOut]
    extractAssetOuts (SomeTxOut txId ix slotNo (TxOut _ value _ _) _ _ _) = case value of
      TxOutAdaOnly _ _  -> []
      TxOutValue _ value' -> do
        (assetId, quantity) <- valueToList value'
        case assetId of
          AdaAssetId            -> []
          AssetId policyId name -> [AssetOut txId ix slotNo policyId name quantity]

headerHashToParam :: Hash BlockHeader -> ByteString
headerHashToParam (HeaderHash hash) = fromShort hash

lovelaceToParam :: Lovelace -> Int64
lovelaceToParam (Lovelace slotNo) = fromIntegral slotNo

slotNoToParam :: SlotNo -> Int64
slotNoToParam (SlotNo slotNo) = fromIntegral $ minimum [slotNo, fromIntegral (maxBound :: Int64)]

txIxToParam :: TxIx -> Int16
txIxToParam (TxIx txIx) = fromIntegral txIx
