{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE QuasiQuotes               #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL where

import Cardano.Api (AddressAny, AsType (..), AssetId (..), AssetName (..), Block (..), BlockHeader (..),
                    BlockInMode (..), BlockNo (..), CardanoMode, ChainPoint (..), CtxTx, EraInMode, IsCardanoEra,
                    PolicyId, Quantity (Quantity), SerialiseAsCBOR (serialiseToCBOR), SerialiseAsRawBytes (..),
                    ShelleyBasedEra (ShelleyBasedEraAlonzo, ShelleyBasedEraBabbage), SlotNo (..), Tx, TxBody (..),
                    TxBodyContent (..), TxId, TxIn (..), TxIx (..), TxMetadata (..), TxMetadataInEra (..),
                    TxMintValue (..), TxOut (..), TxOutDatum (..), TxOutValue (..), TxValidityLowerBound (..),
                    TxValidityUpperBound (..), getTxBody, getTxId, hashScriptData, selectLovelace, valueToList)
import Cardano.Api.Shelley (Hash (..), Tx (..), toShelleyTxIn)

import Cardano.Binary (ToCBOR (toCBOR), toStrictByteString)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort, toShort)
import Data.Foldable (sequenceA_)
import Data.Int (Int64)
import qualified Data.Map as Map
import Data.Profunctor (lmap, rmap)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement, refineResult)
import Hasql.TH (resultlessStatement, vectorStatement)
import qualified Hasql.Transaction as T
import qualified Hasql.Transaction.Sessions as T
import Language.Marlowe.Runtime.ChainSync.Database (CardanoBlock, CommitBlocks (..), CommitGenesisBlock (..),
                                                    CommitRollback (..), GetGenesisBlock (..),
                                                    GetIntersectionPoints (..))
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock (..), GenesisTx (..))

-- GetGenesisBlock

getGenesisBlock :: GetGenesisBlock Session
getGenesisBlock = GetGenesisBlock $ statement () $ refineResult (decodeResults . V.toList)
  [vectorStatement|
    SELECT block.hash :: bytea
         , txOut.txId :: bytea
         , txOut.lovelace :: bigint
         , txOut.address :: bytea
    FROM   chain.block AS block
    JOIN   chain.tx    AS tx    ON tx.blockHash = block.hash
    JOIN   chain.txOut AS txOut ON txOut.txId   = tx.id
    WHERE  block.blockNo IS NULL
    ORDER BY txId ASC
  |]
  where
    decodeResults :: [(ByteString, ByteString, Int64, ByteString)] -> Either Text (Maybe GenesisBlock)
    decodeResults [] = Right Nothing
    decodeResults rows@((hash, _, _, _) : _) =
      Just . GenesisBlock (HeaderHash $ toShort hash) . Set.fromDistinctAscList <$> traverse decodeGenesisTx rows

    decodeGenesisTx :: (ByteString, ByteString, Int64, ByteString) -> Either Text GenesisTx
    decodeGenesisTx (_, txId, lovelace, address) = GenesisTx
      <$> decodeTxId txId
      <*> pure (fromIntegral lovelace)
      <*> decodeAddressAny address

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
    SELECT slotNo :: bigint, hash :: bytea FROM chain.block WHERE slotNo IS NOT NULL ORDER BY slotNo DESC LIMIT 2160
  |]
  where
    decodeResults :: Vector (Int64, ByteString) -> [ChainPoint]
    decodeResults = fmap decodeResult . V.toList

    decodeResult :: (Int64, ByteString) -> ChainPoint
    decodeResult (slotNo, hash) = ChainPoint (SlotNo $ fromIntegral slotNo) (HeaderHash $ toShort hash)

-- CommitRollback

type CommitRollbackParams =
  ( Maybe Int64      -- rollback point slot
  , Maybe ByteString -- rollback point hash
  )

commitRollback :: CommitRollback Session
commitRollback = CommitRollback $ flip statement $ lmap pointToParams stmnt
  where
    stmnt :: Statement CommitRollbackParams ()
    stmnt =
      [resultlessStatement|
        CALL commitRollback
          ( slot => $1 :: bigint?
          , hash => $2 :: bytea?
          )
      |]

    pointToParams :: ChainPoint -> CommitRollbackParams
    pointToParams = \case
      ChainPointAtGenesis                        -> (Nothing, Nothing)
      ChainPoint (SlotNo slot) (HeaderHash hash) -> (Just $ fromIntegral slot, Just $ fromShort hash)

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
  in
    T.transaction T.ReadCommitted T.Write $ sequenceA_
      [ T.statement
          (headerHashToParam genesisBlockHash)
          [resultlessStatement| INSERT INTO chain.block (hash) VALUES ($1 :: bytea) |]
      , T.statement
          ( V.fromList $ serialiseToRawBytes . genesisTxId <$> genesisBlockTxsList
          , V.fromList $ headerHashToParam genesisBlockHash <$ genesisBlockTxsList
          )
          [resultlessStatement|
            INSERT INTO chain.tx (id, blockHash)
            SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[])
          |]
      , T.statement
          ( V.fromList $ serialiseToRawBytes . genesisTxId <$> genesisBlockTxsList
          , V.fromList $ serialiseToRawBytes . genesisTxAddress <$> genesisBlockTxsList
          , V.fromList $ fromIntegral . genesisTxLovelace <$> genesisBlockTxsList
          )
          [resultlessStatement|
            INSERT INTO chain.txOut (txId, address, lovelace, txIx)
            SELECT *, 0 FROM UNNEST ($1 :: bytea[], $2 :: bytea[], $3 :: bigint[])
          |]
      ]

-- CommitBlocks

data BlockRow = BlockRow
  { hash    :: !ByteString
  , slotNo  :: !Int64
  , blockNo :: !Int64
  }

data TxRow = TxRow
  { blockHash          :: !ByteString
  , txId               :: !ByteString
  , validityLowerBound :: !(Maybe Int64)
  , validityUpperBound :: !(Maybe Int64)
  , metadataKey1564    :: !(Maybe ByteString)
  }

data TxOutRow = TxOutRow
  { txId       :: !ByteString
  , txIx       :: !Int64
  , address    :: !ByteString
  , lovelace   :: !Int64
  , datumHash  :: !(Maybe ByteString)
  , datumBytes :: !(Maybe ByteString)
  }

data TxInRow = TxInRow
  { txOutId            :: !ByteString
  , txOutIx            :: !Int64
  , txInId             :: !ByteString
  , redeemerDatumBytes :: !(Maybe ByteString)
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
  , quantity :: !Int64
  }

data AssetMintRow = AssetMintRow
  { policyId :: !ByteString
  , name     :: !ByteString
  , txId     :: !ByteString
  , quantity :: !Int64
  }

data SomeTx = forall era. IsCardanoEra era =>
  SomeTx (Hash BlockHeader) (Tx era) (EraInMode era CardanoMode)

data SomeTxOut = forall era. IsCardanoEra era =>
  SomeTxOut TxId TxIx (TxOut CtxTx era) (EraInMode era CardanoMode)

data SomeTxIn = SomeTxIn TxId TxIx TxId (Maybe ByteString)

data Asset = Asset PolicyId AssetName deriving (Eq, Ord)

data AssetMint = AssetMint TxId PolicyId AssetName Quantity

data AssetOut = AssetOut TxId TxIx PolicyId AssetName Quantity

commitBlocks :: CommitBlocks Session
commitBlocks = CommitBlocks \blocks -> T.transaction T.ReadCommitted T.Write $ sequenceA_
  let
    txs = extractTxs =<< blocks
    txOuts = extractTxOuts =<< txs
    txIns = extractTxIns =<< txs
    assetOuts = extractAssetOuts =<< txOuts
    assetMints = extractAssetMints =<< txs
   in
    [ commitBlockHeaders blocks
    , commitTxs txs
    , commitTxOuts txOuts
    , commitTxIns txIns
    , commitAssets assetOuts assetMints
    ]
  where
    commitBlockHeaders :: [CardanoBlock] -> T.Transaction ()
    commitBlockHeaders blocks = T.statement params stmt
      where
        stmt =
          [resultlessStatement|
            INSERT INTO chain.block (hash, slotNo, blockNo)
            SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bigint[], $3 :: bigint[])
            ON CONFLICT (hash) DO UPDATE SET
                 rollbackToBlock = NULL
               , rollbackToGenesis = NULL
          |]
        params =
          ( V.fromList $ hash <$> rows
          , V.fromList $ slotNo <$> rows
          , V.fromList $ blockNo <$> rows
          )
        rows = row <$> blocks
        row (BlockInMode (Block (BlockHeader slotNo hash (BlockNo blockNo)) _) _)  = BlockRow
          { hash = headerHashToParam hash
          , slotNo = slotNoToParam slotNo
          , blockNo = fromIntegral blockNo
          }

    commitTxs :: [SomeTx] -> T.Transaction ()
    commitTxs txs = T.statement params stmt
      where
        stmt =
          [resultlessStatement|
            INSERT INTO chain.tx (id, blockHash, validityLowerBound, validityUpperBound, metadataKey1564)
            SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[], $3 :: bigint?[], $4 :: bigint?[], $5 :: bytea?[])
            ON CONFLICT (id) DO NOTHING
          |]
        params =
          ( V.fromList $ (\TxRow{..} -> txId) <$> rows
          , V.fromList $ blockHash <$> rows
          , V.fromList $ validityLowerBound <$> rows
          , V.fromList $ validityUpperBound <$> rows
          , V.fromList $ metadataKey1564 <$> rows
          )
        rows = row <$> txs
        row (SomeTx blockHash tx _) = case getTxBody tx of
          body@(TxBody TxBodyContent{..}) -> TxRow
            { blockHash = headerHashToParam blockHash
            , txId = serialiseToRawBytes $ getTxId body
            , validityLowerBound = case fst txValidityRange of
                TxValidityNoLowerBound        -> Nothing
                TxValidityLowerBound _ slotNo -> Just $ slotNoToParam slotNo
            , validityUpperBound = case snd txValidityRange of
                TxValidityNoUpperBound _      -> Nothing
                TxValidityUpperBound _ slotNo -> Just $ slotNoToParam slotNo
            , metadataKey1564 = case txMetadata of
                TxMetadataNone                          -> Nothing
                TxMetadataInEra _ (TxMetadata metadata) -> do
                  value <- Map.lookup 1564 metadata
                  pure $ serialiseToCBOR $ TxMetadata $ Map.singleton 1564 value
            }

    commitTxOuts :: [SomeTxOut] -> T.Transaction ()
    commitTxOuts txOuts = T.statement params stmt
      where
        stmt =
          [resultlessStatement|
            INSERT INTO chain.txOut (txId, txIx, address, lovelace, datumHash, datumBytes)
            SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bigint[], $3 :: bytea[], $4 :: bigint[], $5 :: bytea?[], $6 :: bytea?[])
            ON CONFLICT (txId, txIx) DO NOTHING
          |]
        params =
          ( V.fromList $ (\TxOutRow{..} -> txId) <$> rows
          , V.fromList $ (\TxOutRow{..} -> txIx) <$> rows
          , V.fromList $ address <$> rows
          , V.fromList $ lovelace <$> rows
          , V.fromList $ datumHash <$> rows
          , V.fromList $ datumBytes <$> rows
          )
        rows = row <$> txOuts
        row (SomeTxOut txId txIx (TxOut address value datum _) _) = TxOutRow
          { txId = serialiseToRawBytes txId
          , txIx = txIxToParam txIx
          , address = serialiseToRawBytes address
          , lovelace = fromIntegral case value of
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
          }

    commitTxIns :: [SomeTxIn] -> T.Transaction ()
    commitTxIns txIns = T.statement params stmt
      where
        stmt =
          [resultlessStatement|
            INSERT INTO chain.txIn (txOutId, txOutIx, txInId, redeemerDatumBytes)
            SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bigint[], $3 :: bytea[], $4 :: bytea?[])
            ON CONFLICT (txOutId, txOutIx, txInId) DO NOTHING
          |]
        params =
          ( V.fromList $ txOutId <$> rows
          , V.fromList $ txOutIx <$> rows
          , V.fromList $ txInId <$> rows
          , V.fromList $ redeemerDatumBytes <$> rows
          )
        rows = row <$> txIns
        row (SomeTxIn txOutId txOutIx txInId redeemerDatumBytes) = TxInRow
          { txOutId = serialiseToRawBytes txOutId
          , txOutIx = txIxToParam txOutIx
          , txInId = serialiseToRawBytes txInId
          , redeemerDatumBytes
          }

    commitAssets :: [AssetOut] -> [AssetMint] -> T.Transaction ()
    commitAssets assetOuts assetMints = T.statement params stmt
      where
        stmt =
          [resultlessStatement|
            WITH assetOutInputs (policyId, name, txId, txIx, quantity) AS
              ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[], $3 :: bytea[], $4 :: bigint[], $5 :: bigint[])
              )
            , assetMintInputs (policyId, name, txId, quantity) AS
              ( SELECT * FROM UNNEST ($6 :: bytea[], $7 :: bytea[], $8 :: bytea[], $9 :: bigint[])
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
              ( INSERT INTO chain.assetOut (txId, txIx, assetId, quantity)
                SELECT assetOut.txId, assetOut.txIx, asset.id, assetOut.quantity
                FROM   assetOutInputs AS assetOut
                JOIN   assetIds       AS asset    ON  asset.policyId = assetOut.policyId
                                                  AND asset.name = assetOut.name
              )
            INSERT INTO chain.assetMint (txId, assetId, quantity)
            SELECT assetMint.txId, asset.id, assetMint.quantity
            FROM   assetMintInputs AS assetMint
            JOIN   assetIds        AS asset     ON  asset.policyId = assetMint.policyId
                                                AND asset.name = assetMint.name
          |]
        params =
          ( V.fromList $ (\AssetOutRow{..} -> policyId) <$> assetOutRows
          , V.fromList $ (\AssetOutRow{..} -> name) <$> assetOutRows
          , V.fromList $ (\AssetOutRow{..} -> txId) <$> assetOutRows
          , V.fromList $ (\AssetOutRow{..} -> txIx) <$> assetOutRows
          , V.fromList $ (\AssetOutRow{..} -> quantity) <$> assetOutRows
          , V.fromList $ (\AssetMintRow{..} -> policyId) <$> assetMintRows
          , V.fromList $ (\AssetMintRow{..} -> name) <$> assetMintRows
          , V.fromList $ (\AssetMintRow{..} -> txId) <$> assetMintRows
          , V.fromList $ (\AssetMintRow{..} -> quantity) <$> assetMintRows
          )
        assetOutRows = assetOutRow <$> assetOuts
        assetMintRows = assetMintRow <$> assetMints
        assetOutRow (AssetOut txId txIx policyId (AssetName name) (Quantity quantity)) = AssetOutRow
          { policyId = serialiseToRawBytes policyId
          , name
          , txId = serialiseToRawBytes txId
          , txIx = txIxToParam txIx
          , quantity = fromIntegral quantity
          }
        assetMintRow (AssetMint txId policyId (AssetName name) (Quantity quantity)) = AssetMintRow
          { policyId = serialiseToRawBytes policyId
          , name
          , txId = serialiseToRawBytes txId
          , quantity = fromIntegral quantity
          }

    slotNoToParam :: SlotNo -> Int64
    slotNoToParam (SlotNo slotNo) = fromIntegral slotNo

    txIxToParam :: TxIx -> Int64
    txIxToParam (TxIx txIx) = fromIntegral txIx

    extractTxs :: CardanoBlock -> [SomeTx]
    extractTxs (BlockInMode (Block (BlockHeader _ hash _) blockTxs) era) = flip (SomeTx hash) era <$> blockTxs

    extractTxOuts :: SomeTx -> [SomeTxOut]
    extractTxOuts (SomeTx _ tx era) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> do
        (ix, txOut) <- [0..] `zip` txOuts
        pure $ SomeTxOut (getTxId body) (TxIx ix) txOut era

    extractTxIns :: SomeTx -> [SomeTxIn]
    extractTxIns (SomeTx _ tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> do
        txIn@(TxIn txId txIx) <- fst <$> txIns
        pure $ SomeTxIn txId txIx (getTxId body) $ getRedeemer txIn
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
    extractAssetMints (SomeTx _ tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txMintValue of
        TxMintNone -> []
        TxMintValue _ value _ -> do
          (assetId, quantity) <- valueToList value
          case assetId of
            AdaAssetId            -> []
            AssetId policyId name -> [AssetMint (getTxId body) policyId name quantity]

    extractAssetOuts :: SomeTxOut -> [AssetOut]
    extractAssetOuts (SomeTxOut txId ix (TxOut _ value _ _)  _) = case value of
      TxOutAdaOnly _ _  -> []
      TxOutValue _ value' -> do
        (assetId, quantity) <- valueToList value'
        case assetId of
          AdaAssetId            -> []
          AssetId policyId name -> [AssetOut txId ix policyId name quantity]

headerHashToParam :: Hash BlockHeader -> ByteString
headerHashToParam (HeaderHash hash) = fromShort hash
