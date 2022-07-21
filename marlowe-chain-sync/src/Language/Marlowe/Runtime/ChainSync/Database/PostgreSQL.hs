{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL where

import Cardano.Api (AssetId (..), AssetName (..), Block (..), BlockHeader (..), BlockInMode (..), BlockNo (..),
                    CardanoMode, ChainPoint (..), CtxTx, EraInMode, IsCardanoEra, PolicyId, Quantity (Quantity),
                    SerialiseAsCBOR (serialiseToCBOR), SerialiseAsRawBytes (..),
                    ShelleyBasedEra (ShelleyBasedEraAlonzo, ShelleyBasedEraBabbage), SlotNo (..), Tx, TxBody (..),
                    TxBodyContent (..), TxId, TxIn (..), TxIx (..), TxMetadata (..), TxMetadataInEra (..),
                    TxMintValue (..), TxOut (..), TxOutDatum (..), TxOutValue (..), TxValidityLowerBound (..),
                    TxValidityUpperBound (..), getTxBody, getTxId, hashScriptData, selectLovelace, valueToList)
import Cardano.Api.Shelley (Hash (..), Tx (..))

import Cardano.Binary (ToCBOR (toCBOR), toStrictByteString)
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import Data.ByteString (ByteString)
import Data.ByteString.Short (fromShort)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (sort)
import qualified Data.Map as Map
import Data.Profunctor (lmap)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Hasql.Session (Session, statement)
import Hasql.Statement (Statement)
import Hasql.TH (resultlessStatement)
import Language.Marlowe.Runtime.ChainSync.Database (CardanoBlock, CommitBlocks (..), CommitRollback (..))

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

type CommitBlocksParams =
  ( Vector ByteString           -- block header hashes
  , Vector Int64                -- block slots
  , Vector Int64                -- block numbers

  , Vector ByteString           -- tx block hash (for joining)
  , Vector ByteString           -- tx hash
  , Vector (Maybe Int64)        -- tx validity lower bound
  , Vector (Maybe Int64)        -- tx validity upper bound
  , Vector (Maybe ByteString)  -- tx metadata (key 1564)

  , Vector ByteString           -- tx out tx hash (for joining)
  , Vector Int64                -- tx out ix
  , Vector ByteString           -- tx out address
  , Vector Int64                -- tx out lovelace
  , Vector (Maybe ByteString)   -- tx out datum hash
  , Vector (Maybe ByteString)   -- tx out datum bytes

  , Vector ByteString           -- tx in sink tx hash (for joining)
  , Vector ByteString           -- tx in source tx hash (for joining)
  , Vector Int64                -- tx in tx ix (for joining)
  , Vector (Maybe ByteString)   -- tx in redeemer datum

  , Vector ByteString           -- asset policyId
  , Vector Text                 -- asset name

  , Vector ByteString           -- asset out policyId
  , Vector Text                 -- asset out name
  , Vector ByteString           -- asset out tx hash (for joining)
  , Vector Int64                -- asset out tx ix (for joining)
  , Vector Int64                -- asset out quantity

  , Vector ByteString           -- asset mint policyId
  , Vector Text                 -- asset mint name
  , Vector ByteString           -- asset mint tx hash (for joining)
  , Vector Int64                -- asset mint quantity
  )

data SomeCardanoTx = forall era. IsCardanoEra era =>
  SomeCardanoTx (Hash BlockHeader) (Tx era) (EraInMode era CardanoMode)

data SomeCardanoTxOut = forall era. IsCardanoEra era =>
  SomeCardanoTxOut TxId TxIx (TxOut CtxTx era) (EraInMode era CardanoMode)

data Asset = Asset PolicyId AssetName deriving (Eq, Ord)

data MintAsset = MintAsset TxId PolicyId AssetName Quantity

data OutAsset = OutAsset TxId TxIx PolicyId AssetName Quantity

commitBlocks :: CommitBlocks Session
commitBlocks = CommitBlocks $ flip statement $ lmap blocksToParams stmnt
  where
    stmnt :: Statement CommitBlocksParams ()
    stmnt =
      [resultlessStatement|
        WITH blockInputs (hash, slotNo, blockNo) AS
          ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bigint[], $3 :: bigint[])
          )
        , txInputs (blockHash, hash, validityLowerBound, validityLowerBound, metadataKey1564) AS
          ( SELECT * FROM UNNEST ($4 :: bytea[], $5 :: bytea[], $6 :: bigint?[], $7 :: bigint?[], $8 :: bytea?[])
          )
        , txOutInputs (txHash, ix, address, lovelace, datumHash, datumBytes) AS
          ( SELECT * FROM UNNEST ($9 :: bytea[], $10 :: bigint[], $11 :: bytea[], $12 :: bigint[], $13 :: bytea?[], $14 :: bytea?[])
          )
        , txInInputs (txSink, txSource, ix, redeemerDatumBytes) AS
          ( SELECT * FROM UNNEST ($15 :: bytea[], $16 :: bytea[], $17 :: bigint[], $18 :: bytea?[])
          )
        , assetInputs (policyId, name) AS
          ( SELECT * FROM UNNEST ($19 :: bytea[], $20 :: text[])
          )
        , assetOutInputs (policyId, name, txHash, ix, quantity) AS
          ( SELECT * FROM UNNEST ($21 :: bytea[], $22 :: text[], $23 :: bytea[], $24 :: bigint[], $25 :: bigint[])
          )
        , assetMintInputs (policyId, name, txHash, quantity) AS
          ( SELECT * FROM UNNEST ($26 :: bytea[], $27 :: text[], $28 :: bytea[], $29 :: bigint[])
          )
        , newBlocks AS
          ( INSERT INTO chain.block (hash, slotNo, blockNo)
            SELECT * FROM blockInputs
            ON CONFLICT (hash) DO UPDATE SET rollbackBlock = NULL
            RETURNING (id, hash)
          )
        , newTxs AS
          ( INSERT INTO chain.tx (blockId, hash, validityLowerBound, validityUpperBound, metadataKey1564)
            SELECT block.id, tx.hash, tx.validityLowerBound, tx.validityUpperBound, tx.metadataKey1564
            FROM   txInputs  AS tx
            JOIN   newBlocks AS block ON tx.blockHash = block.hash
            RETURNING (id, hash)
          )
        , txIds AS
          ( SELECT id, hash
            FROM   newTxs
            UNION  ALL
            SELECT tx.id, tx.hash
            FROM   txInInputs AS txIn
            JOIN   chain.tx   AS tx   ON tx.hash = txIn.txSource
          )
        , newTxOuts AS
          ( INSERT INTO chain.txOut (txId, ix, address, lovelace, datumHash, datumBytes)
            SELECT tx.id, txOut.ix, txOut.address, txOut.lovelace, txOut.datumHash, txOut.datumBytes
            FROM   txOutInputs AS txOut
            JOIN   txIds       AS tx    ON txOut.txHash = tx.hash
            RETURNING (id, txOut.txHash, ix)
          )
        , txOutIds AS
          ( SELECT id, txHash, ix
            FROM   newTxOuts AS txOut
            UNION  ALL
            SELECT txOut.id, tx.hash, txOut.ix
            FROM   txInInputs  AS txIn
            JOIN   chain.tx    AS tx    ON  tx.hash = txIn.txSource
            JOIN   chain.txOut AS txOut ON  txOut.txId = tx.id
                                        AND txOut.ix = txIn.ix
          )
        , newTxIns AS
          ( INSERT INTO chain.txIn (txId, txOutId, redeemerDatumBytes)
            SELECT tx.id, txOut.id, txIn.redeemerDatumBytes
            FROM  txInInputs AS txIn
            JOIN  newTxs     AS tx    ON  tx.hash = txIn.txSink
            JOIN  txOutIds   AS txOut ON  txOut.txHash = txIn.txSource
                                      AND txOut.ix = txIn.ix
          )
        , newAssets AS
          ( INSERT INTO chain.asset (policyId, name)
            SELECT * FROM assetInputs
            ON CONFLICT (policyId, name) DO NOTHING
            RETURNING (id, policyId, name)
          )
        , assetIds AS
          ( SELECT * FROM newAssets
            UNION ALL
            SELECT asset.*
            FROM   assetOutInputs AS assetOut
            JOIN   chain.asset    AS asset    ON  asset.policyId = assetOut.policuId
                                              AND asset.name = assetOut.name
            UNION ALL
            SELECT asset.*
            FROM   assetMintInputs AS assetMint
            JOIN   chain.asset    AS asset    ON  asset.policyId = assetMint.policuId
                                              AND asset.name = assetMint.name
          )
        , newAssetOuts AS
          ( INSERT INTO chain.assetOut (txOutId, assetId, quantity)
            SELECT txOut.id, asset.id, assetOut.quantity
            FROM   assetOutInputs AS assetOut
            JOIN   txOutIds       AS txOut    ON  txOut.txHash = assetOut.txHash
                                              AND txOut.ix = assetOut.ix
            JOIN   assetIds       AS asset    ON  asset.policyId = assetOut.policyId
                                              AND asset.name = assetOut.name
          )
        INSERT INTO chain.assetMint (txId, assetId, quantity)
        SELECT tx.id, asset.id, assetMint.quantity
        FROM   assetMintInputs AS assetMint
        JOIN   txIds           AS tx        ON  tx.hash = assetMint.txHash
        JOIN   assetIds        AS asset     ON  asset.policyId = assetMint.policyId
                                            AND asset.name = assetMint.name
      |]

    blocksToParams :: [CardanoBlock] -> CommitBlocksParams
    blocksToParams blocks =
      ( V.fromList $ extractHeaderHash           <$> blocks
      , V.fromList $ extractSlot                 <$> blocks
      , V.fromList $ extractBlockNo              <$> blocks

      , V.fromList $ extractTxHeaderHash         <$> txs
      , V.fromList $ extractTxHash               <$> txs
      , V.fromList $ extractTxValidityLowerBound <$> txs
      , V.fromList $ extractTxValidityUpperBound <$> txs
      , V.fromList $ extractTxMetadataKey1564    <$> txs

      , V.fromList $ extractTxOutTxHash          <$> txOuts
      , V.fromList $ extractTxOutIx              <$> txOuts
      , V.fromList $ extractTxOutAddress         <$> txOuts
      , V.fromList $ extractTxOutLovelace        <$> txOuts
      , V.fromList $ extractTxOutDatumHash       <$> txOuts
      , V.fromList $ extractTxOutDatumBytes      <$> txOuts

      , V.fromList $ extractTxInSinkTxHashes     =<< txs
      , V.fromList $ extractTxInSourceTxHashes   =<< txs
      , V.fromList $ extractTxInIxes             =<< txs
      , V.fromList $ extractTxInRedeemerBytes    =<< txs

      , V.fromList $ extractAssetPolicyId        <$> assets
      , V.fromList $ extractAssetName            <$> assets

      , V.fromList $ extractAssetOutPolicyId     <$> outAssets
      , V.fromList $ extractAssetOutName         <$> outAssets
      , V.fromList $ extractAssetOutTxHash       <$> outAssets
      , V.fromList $ extractAssetOutIx           <$> outAssets
      , V.fromList $ extractAssetOutQuantity     <$> outAssets

      , V.fromList $ extractAssetMintPolicyId    <$> mintAssets
      , V.fromList $ extractAssetMintName        <$> mintAssets
      , V.fromList $ extractAssetMintTxHash      <$> mintAssets
      , V.fromList $ extractAssetMintQuantity    <$> mintAssets
      )
      where
        txs :: [SomeCardanoTx]
        txs = extractTxs =<< blocks

        txOuts :: [SomeCardanoTxOut]
        txOuts = extractTxOuts =<< txs

        assets :: [Asset]
        assets = Set.toList
          $ on (<>) Set.fromList (mintAssetToAsset <$> mintAssets) (outAssetToAsset <$> outAssets)

        mintAssets :: [MintAsset]
        mintAssets = extractMintAssets =<< txs

        outAssets :: [OutAsset]
        outAssets = extractOutAssets =<< txOuts

    extractHeaderHash :: CardanoBlock -> ByteString
    extractHeaderHash (BlockInMode (Block (BlockHeader _ (HeaderHash hash) _) _) _) = fromShort hash

    extractSlot :: CardanoBlock -> Int64
    extractSlot (BlockInMode (Block (BlockHeader (SlotNo slot) _ _) _) _) = fromIntegral slot

    extractBlockNo :: CardanoBlock -> Int64
    extractBlockNo (BlockInMode (Block (BlockHeader _ _ (BlockNo  blockNo)) _) _) = fromIntegral blockNo

    extractTxs :: CardanoBlock -> [SomeCardanoTx]
    extractTxs (BlockInMode (Block (BlockHeader _ hash _) blockTxs) era) = flip (SomeCardanoTx hash) era <$> blockTxs

    extractTxHeaderHash :: SomeCardanoTx -> ByteString
    extractTxHeaderHash (SomeCardanoTx (HeaderHash hash) _ _) = fromShort hash

    extractTxHash :: SomeCardanoTx -> ByteString
    extractTxHash (SomeCardanoTx _ tx _) = serialiseToRawBytes $ getTxId $ getTxBody tx

    extractTxValidityLowerBound :: SomeCardanoTx -> Maybe Int64
    extractTxValidityLowerBound (SomeCardanoTx _ tx _) = case getTxBody tx of
      TxBody TxBodyContent{..} -> case fst txValidityRange of
        TxValidityNoLowerBound               -> Nothing
        TxValidityLowerBound _ (SlotNo slot) -> Just $ fromIntegral slot

    extractTxValidityUpperBound :: SomeCardanoTx -> Maybe Int64
    extractTxValidityUpperBound (SomeCardanoTx _ tx _) = case getTxBody tx of
      TxBody TxBodyContent{..} -> case snd txValidityRange of
        TxValidityNoUpperBound _             -> Nothing
        TxValidityUpperBound _ (SlotNo slot) -> Just $ fromIntegral slot

    extractTxMetadataKey1564 :: SomeCardanoTx -> Maybe ByteString
    extractTxMetadataKey1564 (SomeCardanoTx _ tx _) = case getTxBody tx of
      TxBody TxBodyContent{..} -> case txMetadata of
        TxMetadataNone                          -> Nothing
        TxMetadataInEra _ (TxMetadata metadata) -> do
          value <- Map.lookup 1564 metadata
          pure $ serialiseToCBOR $ TxMetadata $ Map.singleton 1564 value

    extractTxOuts :: SomeCardanoTx -> [SomeCardanoTxOut]
    extractTxOuts (SomeCardanoTx _ tx era) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> do
        (ix, txOut) <- [0..] `zip` txOuts
        pure $ SomeCardanoTxOut (getTxId body) (TxIx ix) txOut era

    extractTxOutTxHash :: SomeCardanoTxOut -> ByteString
    extractTxOutTxHash (SomeCardanoTxOut txId _ _ _) = serialiseToRawBytes txId

    extractTxOutIx :: SomeCardanoTxOut -> Int64
    extractTxOutIx (SomeCardanoTxOut _ (TxIx ix) _ _) = fromIntegral ix

    extractTxOutAddress :: SomeCardanoTxOut -> ByteString
    extractTxOutAddress (SomeCardanoTxOut _ _ (TxOut address _ _ _) _) = serialiseToRawBytes address

    extractTxOutLovelace :: SomeCardanoTxOut -> Int64
    extractTxOutLovelace (SomeCardanoTxOut _ _ (TxOut _ value _ _) _) = fromIntegral case value of
      TxOutAdaOnly _ lovelace -> lovelace
      TxOutValue _ value'     -> selectLovelace value'

    extractTxOutDatumHash :: SomeCardanoTxOut -> Maybe ByteString
    extractTxOutDatumHash (SomeCardanoTxOut _ _ (TxOut _ _ datum _) _) = case datum of
      TxOutDatumNone        -> Nothing
      TxOutDatumHash _ hash -> Just $ serialiseToRawBytes hash
      TxOutDatumInTx _ d    -> Just $ serialiseToRawBytes $ hashScriptData d
      TxOutDatumInline _ d  -> Just $ serialiseToRawBytes $ hashScriptData d

    extractTxOutDatumBytes :: SomeCardanoTxOut -> Maybe ByteString
    extractTxOutDatumBytes (SomeCardanoTxOut _ _ (TxOut _ _ datum _) _) = case datum of
      TxOutDatumNone       -> Nothing
      TxOutDatumHash _ _   -> Nothing
      TxOutDatumInTx _ d   -> Just $ serialiseToCBOR d
      TxOutDatumInline _ d -> Just $ serialiseToCBOR d

    extractTxInSourceTxHashes :: SomeCardanoTx -> [ByteString]
    extractTxInSourceTxHashes (SomeCardanoTx _ tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> serialiseToRawBytes (getTxId body) <$ txIns

    extractTxInSinkTxHashes :: SomeCardanoTx -> [ByteString]
    extractTxInSinkTxHashes (SomeCardanoTx _ tx _) = case getTxBody tx of
      TxBody TxBodyContent{..} -> do
        TxIn txId _ <- sort $ fst <$> txIns
        pure $ serialiseToRawBytes txId

    extractTxInIxes :: SomeCardanoTx -> [Int64]
    extractTxInIxes (SomeCardanoTx _ tx _) = case getTxBody tx of
      TxBody TxBodyContent{..} -> do
        TxIn _ (TxIx ix) <- sort $ fst <$> txIns
        pure $ fromIntegral ix

    extractTxInRedeemerBytes :: SomeCardanoTx -> [Maybe ByteString]
    extractTxInRedeemerBytes (SomeCardanoTx _ tx _) = case tx of
      ShelleyTx ShelleyBasedEraAlonzo alonzoTx@Alonzo.ValidatedTx{} -> do
        txIn <- sort . Set.toList . Alonzo.inputs . Alonzo.body $ alonzoTx
        pure do
          (Alonzo.Data datum, _) <- Alonzo.indexedRdmrs alonzoTx $ Alonzo.Spending txIn
          pure $ toStrictByteString $ toCBOR datum

      ShelleyTx ShelleyBasedEraBabbage  babbageTx@Babbage.ValidatedTx{} -> do
        txIn <- sort . Set.toList . Babbage.inputs . Babbage.body $ babbageTx
        pure do
          (Alonzo.Data datum, _) <- Babbage.indexedRdmrs babbageTx $ Babbage.Spending txIn
          pure $ toStrictByteString $ toCBOR datum

      _ -> case getTxBody tx of
        TxBody TxBodyContent{..} -> Nothing <$ txIns

    extractMintAssets :: SomeCardanoTx -> [MintAsset]
    extractMintAssets (SomeCardanoTx _ tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txMintValue of
        TxMintNone -> []
        TxMintValue _ value _ -> do
          (assetId, quantity) <- valueToList value
          case assetId of
            AdaAssetId            -> []
            AssetId policyId name -> [MintAsset (getTxId body) policyId name quantity]

    mintAssetToAsset :: MintAsset -> Asset
    mintAssetToAsset (MintAsset _ policyId name _) = Asset policyId name

    extractOutAssets :: SomeCardanoTxOut -> [OutAsset]
    extractOutAssets (SomeCardanoTxOut txId ix (TxOut _ value _ _)  _) = case value of
      TxOutAdaOnly _ _  -> []
      TxOutValue _ value' -> do
        (assetId, quantity) <- valueToList value'
        case assetId of
          AdaAssetId            -> []
          AssetId policyId name -> [OutAsset txId ix policyId name quantity]

    outAssetToAsset :: OutAsset -> Asset
    outAssetToAsset (OutAsset _ _ policyId name _) = Asset policyId name

    extractAssetPolicyId :: Asset -> ByteString
    extractAssetPolicyId (Asset policyId _) = serialiseToRawBytes policyId

    extractAssetName :: Asset -> Text
    extractAssetName (Asset _ (AssetName name)) = decodeUtf8 name

    extractAssetOutPolicyId :: OutAsset -> ByteString
    extractAssetOutPolicyId (OutAsset _ _ policyId _ _) = serialiseToRawBytes policyId

    extractAssetOutName :: OutAsset -> Text
    extractAssetOutName (OutAsset _ _ _ (AssetName name) _) = decodeUtf8 name

    extractAssetOutTxHash :: OutAsset -> ByteString
    extractAssetOutTxHash (OutAsset txId _ _ _ _) = serialiseToRawBytes txId

    extractAssetOutIx :: OutAsset -> Int64
    extractAssetOutIx (OutAsset _ (TxIx ix) _ _ _) = fromIntegral ix

    extractAssetOutQuantity :: OutAsset -> Int64
    extractAssetOutQuantity (OutAsset _ _ _ _ (Quantity q)) = fromIntegral q

    extractAssetMintPolicyId :: MintAsset -> ByteString
    extractAssetMintPolicyId (MintAsset _ policyId _ _) = serialiseToRawBytes policyId

    extractAssetMintName :: MintAsset -> Text
    extractAssetMintName (MintAsset _ _ (AssetName name) _) = decodeUtf8 name

    extractAssetMintTxHash :: MintAsset -> ByteString
    extractAssetMintTxHash (MintAsset txId _ _ _) = serialiseToRawBytes txId

    extractAssetMintQuantity :: MintAsset -> Int64
    extractAssetMintQuantity (MintAsset _ _ _ (Quantity q)) = fromIntegral q
