{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE QuasiQuotes               #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL where

import Cardano.Api (AddressAny, AsType (..), AssetId (..), AssetName (..), Block (..), BlockHeader (..),
                    BlockInMode (..), BlockNo (..), CardanoMode, ChainPoint (..), CtxTx, EraInMode, IsCardanoEra,
                    Lovelace (..), PolicyId, Quantity (Quantity), ScriptValidity (..),
                    SerialiseAsCBOR (serialiseToCBOR), SerialiseAsRawBytes (..),
                    ShelleyBasedEra (ShelleyBasedEraAlonzo, ShelleyBasedEraBabbage), SlotNo (..), Tx, TxBody (..),
                    TxBodyContent (..), TxId, TxIn (..), TxInsCollateral (..), TxIx (..), TxMetadata (..),
                    TxMetadataInEra (..), TxMintValue (..), TxOut (..), TxOutDatum (..), TxOutValue (..),
                    TxReturnCollateral (..), TxScriptValidity (..), TxValidityLowerBound (..),
                    TxValidityUpperBound (..), getTxBody, getTxId, hashScriptData, selectLovelace, valueToList)
import Cardano.Api.Shelley (Hash (..), Tx (..), toShelleyTxIn)

import Cardano.Binary (ToCBOR (toCBOR), toStrictByteString)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Short (fromShort, toShort)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Profunctor (rmap)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.Session (Session, sql, statement)
import Hasql.Statement (refineResult)
import Hasql.TH (maybeStatement, resultlessStatement, vectorStatement)
import Language.Marlowe.Runtime.ChainSync.Database (CardanoBlock, CommitBlocks (..), CommitGenesisBlock (..),
                                                    CommitRollback (..), GetGenesisBlock (..), GetHeaderAtPoint (..),
                                                    GetIntersectionPoints (..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock (..), GenesisTx (..))
import Ouroboros.Network.Point (WithOrigin (..))

-- GetGenesisBlock

getGenesisBlock :: GetGenesisBlock Session
getGenesisBlock = GetGenesisBlock $ statement () $ refineResult (decodeResults . V.toList)
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

getHeaderAtPoint :: GetHeaderAtPoint Session
getHeaderAtPoint = GetHeaderAtPoint \case
  ChainPointAtGenesis -> pure Origin
  point@(ChainPoint slotNo hash) ->
    statement (slotNoToParam slotNo, headerHashToParam hash) $ refineResult decodeResults
      [maybeStatement|
        SELECT block.blockNo :: bigint
          FROM chain.block AS block
        WHERE block.slotNo = $1 :: bigint
          AND block.id     = $2 :: bytea
      |]
    where
      decodeResults :: Maybe Int64 -> Either Text (WithOrigin BlockHeader)
      decodeResults Nothing        = Left $ "No block found at " <> T.pack (show point)
      decodeResults (Just blockNo) = Right $ At $ BlockHeader slotNo hash $ BlockNo $ fromIntegral blockNo

decodeTxId :: ByteString -> Either Text TxId
decodeTxId txId = case deserialiseFromRawBytes AsTxId txId of
  Nothing    -> Left "Invalid TxId"
  Just txId' -> Right txId'

decodeAddressAny :: ByteString -> Either Text AddressAny
decodeAddressAny address = case deserialiseFromRawBytes AsAddressAny address of
  Nothing       -> Left "Invalid address"
  Just address' -> Right address'

-- GetIntersectionPoints

getIntersectionPoints :: GetIntersectionPoints Session
getIntersectionPoints = GetIntersectionPoints \_ _ -> statement () $ rmap decodeResults
  [vectorStatement|
    SELECT slotNo :: bigint, id :: bytea
    FROM   chain.block
    WHERE  rollbackToBlock IS NULL
    ORDER BY slotNo DESC LIMIT 2160
  |]
  where
    decodeResults :: Vector (Int64, ByteString) -> [ChainPoint]
    decodeResults = fmap decodeResult . V.toList

    decodeResult :: (Int64, ByteString) -> ChainPoint
    decodeResult (slotNo, hash) = ChainPoint (SlotNo $ fromIntegral slotNo) (HeaderHash $ toShort hash)

-- CommitRollback

commitRollback :: CommitRollback Session
commitRollback = CommitRollback \case
  ChainPointAtGenesis -> sql $ BS.intercalate "\n"
    [ "BEGIN;"
    , "TRUNCATE TABLE chain.tx;"
    , "TRUNCATE TABLE chain.txOut;"
    , "TRUNCATE TABLE chain.txIn;"
    , "DELETE FROM chain.asset;"
    , "TRUNCATE TABLE chain.assetOut;"
    , "TRUNCATE TABLE chain.assetMint;"
    , "TRUNCATE TABLE chain.block;"
    , "COMMIT;"
    ]
  ChainPoint slotNo hash -> statement (slotNoToParam slotNo, headerHashToParam hash)
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
type GenesisBlockParams =
  ( ByteString
  , Vector ByteString
  , Vector Int64
  , Vector ByteString
  )

commitGenesisBlock :: CommitGenesisBlock Session
commitGenesisBlock = CommitGenesisBlock \GenesisBlock{..} ->
  let
    genesisBlockTxsList = Set.toList genesisBlockTxs
    params =
      ( headerHashToParam genesisBlockHash
      , V.fromList $ serialiseToRawBytes . genesisTxId <$> genesisBlockTxsList
      , V.fromList $ serialiseToRawBytes . genesisTxAddress <$> genesisBlockTxsList
      , V.fromList $ lovelaceToParam . genesisTxLovelace <$> genesisBlockTxsList
      )
  in
    statement params
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
        INSERT INTO chain.txOut (txId, address, lovelace, slotNo, txIx, isCollateral)
        SELECT *, -1, 0, false FROM UNNEST ($2 :: bytea[], $3 :: bytea[], $4 :: bigint[])
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
  , metadataKey1564    :: !(Maybe ByteString)
  , isValid            :: !Bool
  }

data TxOutRow = TxOutRow
  { txId         :: !ByteString
  , txIx         :: !Int64
  , slotNo       :: !Int64
  , address      :: !ByteString
  , lovelace     :: !Int64
  , datumHash    :: !(Maybe ByteString)
  , datumBytes   :: !(Maybe ByteString)
  , isCollateral :: !Bool
  }

data TxInRow = TxInRow
  { txOutId            :: !ByteString
  , txOutIx            :: !Int64
  , txInId             :: !ByteString
  , slotNo             :: !Int64
  , redeemerDatumBytes :: !(Maybe ByteString)
  , isCollateral       :: !Bool
  }

data AssetRow = AssetRow
  { policyId :: !ByteString
  , name     :: !ByteString
  }

data AssetOutRow = AssetOutRow
  { policyId :: !ByteString
  , name     :: !ByteString
  , txId     :: !ByteString
  , txIx     :: !Int64
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
  SomeTxOut TxId TxIx SlotNo (TxOut CtxTx era) Bool (EraInMode era CardanoMode)

data SomeTxIn = SomeTxIn TxId TxIx TxId SlotNo (Maybe ByteString) Bool

data Asset = Asset PolicyId AssetName deriving (Eq, Ord)

data AssetMint = AssetMint TxId SlotNo PolicyId AssetName Quantity

data AssetOut = AssetOut TxId TxIx SlotNo PolicyId AssetName Quantity

-- TODO use COPY ... FROM when catching up for faster bulk inserts.
-- See https://hackage.haskell.org/package/postgresql-libpq-0.9.4.3/docs/Database-PostgreSQL-LibPQ.html#g:8
-- For how to do this with postgresql-libpg. You can expose the underlying
-- libpg connection from a `Session` using `withLibPQConnection`
-- (see https://hackage.haskell.org/package/hasql-1.6.0.1/docs/Hasql-Connection.html#v:withLibPQConnection)
commitBlocks :: CommitBlocks Session
commitBlocks = CommitBlocks \blocks _ _ ->
  let
    txs = extractTxs =<< blocks
    txOuts = extractTxOuts =<< txs
    txIns = extractTxIns =<< txs
    assetOuts = extractAssetOuts =<< txOuts
    assetMints = extractAssetMints =<< txs
   in
    statement (params blocks txs txOuts txIns assetOuts assetMints)
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
             WHERE chain.block.id = input.id AND chain.block.slotNo = input.blockNo
          )
        , txInputs (id, blockId, slotNo, validityLowerBound, validityUpperBound, metadataKey1564, isValid) AS
          ( SELECT * FROM UNNEST ($4 :: bytea[], $5 :: bytea[], $6 :: bigint[], $7 :: bigint?[], $8 :: bigint?[], $9 :: bytea?[], $10 :: boolean[])
          )
        , newTxs AS
          ( INSERT INTO chain.tx (id, blockId, slotNo, validityLowerBound, validityUpperBound, metadataKey1564, isValid)
            SELECT tx.id, tx.blockId, tx.slotNo, tx.validityUpperBound, tx.validityUpperBound, tx.metadataKey1564, tx.isValid
            FROM   txInputs AS tx
          )
        , txOutInputs (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral) AS
          ( SELECT * FROM UNNEST ($11 :: bytea[], $12 :: bigint[], $13 :: bigint[], $14 :: bytea[], $15 :: bigint[], $16 :: bytea?[], $17 :: bytea?[], $18 :: boolean[])
          )
        , newTxOuts AS
          ( INSERT INTO chain.txOut (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral)
            SELECT txOut.txId, txOut.txIx, txOut.slotNo, txOut.address, txOut.lovelace, txOut.datumHash, txOut.datumBytes, txOut.isCollateral
            FROM   txOutInputs AS txOut
          )
        , txInInputs (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral) AS
          ( SELECT * FROM UNNEST ($19 :: bytea[], $20 :: bigint[], $21 :: bytea[], $22 :: bigint[], $23 :: bytea?[], $24 :: boolean[])
          )
        , newTxIns AS
          ( INSERT INTO chain.txIn (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral)
            SELECT txIn.txOutId, txIn.txOutIx, txIn.txInId, txIn.slotNo, txIn.redeemerDatumBytes, txIn.isCollateral
            FROM   txInInputs  AS txIn
          )
        , assetOutInputs (policyId, name, txOutId, txOutIx, slotNo, quantity) AS
          ( SELECT * FROM UNNEST ($25 :: bytea[], $26 :: bytea[], $27 :: bytea[], $28 :: bigint[], $29 :: bigint[], $30 :: bigint[])
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
            JOIN   chain.asset    AS asset    ON  asset.policyId = assetOut.policyId
                                              AND asset.name = assetOut.name
            UNION
            SELECT asset.*
            FROM   assetMintInputs AS assetMint
            JOIN   chain.asset    AS asset    ON  asset.policyId = assetMint.policyId
                                              AND asset.name = assetMint.name
          )
        , newAssetOuts AS
          ( INSERT INTO chain.assetOut (txOutId, txOutIx, slotNo, assetId, quantity)
            SELECT assetOut.txOutId, assetOut.txOutIx, assetOut.slotNo, asset.id, assetOut.quantity
            FROM   assetOutInputs AS assetOut
            JOIN   assetIds       AS asset    ON  asset.policyId = assetOut.policyId
                                              AND asset.name = assetOut.name
          )
        INSERT INTO chain.assetMint (txId, slotNo, assetId, quantity)
        SELECT assetMint.txId, assetMint.slotNo, asset.id, assetMint.quantity
        FROM   assetMintInputs AS assetMint
        JOIN   assetIds        AS asset     ON  asset.policyId = assetMint.policyId
                                            AND asset.name = assetMint.name
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
      , V.fromList $ metadataKey1564 <$> txRows
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
            , metadataKey1564 = case txMetadata of
                TxMetadataNone                          -> Nothing
                TxMetadataInEra _ (TxMetadata metadata) -> do
                  value <- Map.lookup 1564 metadata
                  pure $ serialiseToCBOR $ TxMetadata $ Map.singleton 1564 value
            , isValid = case txScriptValidity of
                TxScriptValidity _ ScriptInvalid -> False
                _                                -> True
            }

        txOutRows = txOutRow <$> txOuts
        txOutRow (SomeTxOut txId txIx slotNo (TxOut address value datum _) isCollateral _) = TxOutRow
          { txId = serialiseToRawBytes txId
          , txIx = txIxToParam txIx
          , slotNo = slotNoToParam slotNo
          , address = serialiseToRawBytes address
          , lovelace = lovelaceToParam case value of
              TxOutAdaOnly _ lovelace -> lovelace
              TxOutValue _ value'     -> selectLovelace value'
          , datumHash = case datum of
              TxOutDatumNone        -> Nothing
              TxOutDatumHash _ hash -> Just $ serialiseToRawBytes hash
              TxOutDatumInTx _ d    -> Just $ serialiseToRawBytes $ hashScriptData d
              TxOutDatumInline _ d  -> Just $ serialiseToRawBytes $ hashScriptData d
          , datumBytes = case datum of
              TxOutDatumNone       -> Nothing
              TxOutDatumHash _ _   -> Nothing
              TxOutDatumInTx _ d   -> Just $ serialiseToCBOR d
              TxOutDatumInline _ d -> Just $ serialiseToCBOR d
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
          TxReturnCollateral _ txOut -> [SomeTxOut (getTxId body) (TxIx 0) slotNo txOut True era]
        _ -> do
          (ix, txOut) <- [0..] `zip` txOuts
          pure $ SomeTxOut (getTxId body) (TxIx ix) slotNo txOut False era

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
              (Alonzo.Data datum, _) <- Alonzo.indexedRdmrs alonzoTx $ Alonzo.Spending $ toShelleyTxIn txIn
              pure $ toStrictByteString $ toCBOR datum

          ShelleyTx ShelleyBasedEraBabbage  babbageTx@Babbage.ValidatedTx{} -> do
            \txIn -> do
              (Alonzo.Data datum, _) <- Babbage.indexedRdmrs babbageTx $ Babbage.Spending $ toShelleyTxIn txIn
              pure $ toStrictByteString $ toCBOR datum

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
    extractAssetOuts (SomeTxOut txId ix slotNo (TxOut _ value _ _)  _ _) = case value of
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
slotNoToParam (SlotNo slotNo) = fromIntegral slotNo

txIxToParam :: TxIx -> Int64
txIxToParam (TxIx txIx) = fromIntegral txIx
