{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL
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
  , TxMetadata(..)
  , TxMetadataInEra(..)
  , TxMintValue(..)
  , TxOut(..)
  , TxOutDatum(..)
  , TxOutValue(..)
  , TxReturnCollateral(..)
  , TxScriptValidity(..)
  , TxValidityLowerBound(..)
  , TxValidityUpperBound(..)
  , getTxBody
  , getTxId
  , hashScriptData
  , selectLovelace
  , valueToList
  )
import Cardano.Api.Shelley (Hash(..), Tx(..), toPlutusData, toShelleyTxIn)
import Cardano.Binary (ToCBOR(toCBOR), toStrictByteString, unsafeDeserialize')
import qualified Cardano.Ledger.Alonzo.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import Control.Foldl (Fold(Fold))
import qualified Control.Foldl as Fold
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Short (fromShort, toShort)
import Data.Int (Int16, Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Sum(..))
import Data.Profunctor (rmap)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.These (These(..))
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void)
import Hasql.Session (Session)
import Hasql.Statement (refineResult)
import Hasql.TH (foldStatement, maybeStatement, resultlessStatement, singletonStatement, vectorStatement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as TS
import qualified Language.Marlowe.Runtime.ChainSync.Api as Api
import Language.Marlowe.Runtime.ChainSync.Database
  ( CardanoBlock
  , CommitBlocks(..)
  , CommitGenesisBlock(..)
  , CommitRollback(..)
  , DatabaseQueries(DatabaseQueries)
  , GetGenesisBlock(..)
  , GetHeaderAtPoint(..)
  , GetIntersectionPoints(..)
  , MoveClient(..)
  , MoveResult(..)
  , hoistCommitBlocks
  , hoistCommitGenesisBlock
  , hoistCommitRollback
  , hoistGetGenesisBlock
  , hoistGetHeaderAtPoint
  , hoistGetIntersectionPoints
  , hoistMoveClient
  )
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock(..), GenesisTx(..))
import Numeric.Natural (Natural)
import Ouroboros.Network.Point (WithOrigin(..))
import Prelude hiding (init)

-- | PostgreSQL implementation for the chain sync database queries.
databaseQueries :: GenesisBlock -> DatabaseQueries Session
databaseQueries genesisBlock = DatabaseQueries
  (hoistCommitRollback (TS.transaction TS.ReadCommitted TS.Write) $ commitRollback genesisBlock)
  (hoistCommitBlocks (TS.transaction TS.ReadCommitted TS.Write) commitBlocks)
  (hoistCommitGenesisBlock (TS.transaction TS.ReadCommitted TS.Write) commitGenesisBlock)
  (hoistGetHeaderAtPoint (TS.transaction TS.ReadCommitted TS.Read) getHeaderAtPoint)
  (hoistGetIntersectionPoints (TS.transaction TS.ReadCommitted TS.Read) getIntersectionPoints)
  (hoistGetGenesisBlock (TS.transaction TS.ReadCommitted TS.Read) getGenesisBlock)
  (hoistMoveClient (TS.transaction TS.ReadCommitted TS.Read) moveClient)

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

-- GetHeaderAtPoint

getHeaderAtPoint :: GetHeaderAtPoint Transaction
getHeaderAtPoint = GetHeaderAtPoint \case
  ChainPointAtGenesis -> pure Origin
  point@(ChainPoint slotNo hash) ->
    HT.statement (slotNoToParam slotNo, headerHashToParam hash) $ refineResult decodeResults
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

-- MoveClient

moveClient :: MoveClient Transaction
moveClient = MoveClient performMoveWithRollbackCheck

performMoveWithRollbackCheck :: Api.ChainPoint -> Api.Move err result -> Transaction (MoveResult err result)
performMoveWithRollbackCheck point move = do
  tip <- getTip
  getRollbackPoint >>= \case
    Nothing -> performMove move point >>= \case
      MoveAbort err            -> pure $ Reject err tip
      MoveArrive point' result -> pure $ RollForward result point' tip
      MoveWait                 -> pure $ Wait tip
    Just rollbackPoint -> pure $ RollBack rollbackPoint tip
  where
    getRollbackPoint :: Transaction (Maybe Api.ChainPoint)
    getRollbackPoint = case pointParams of
      Nothing -> pure Nothing
      Just pointParams' -> fmap decodeChainPoint <$> HT.statement pointParams'
        [maybeStatement|
          WITH RECURSIVE rollbacks AS
            ( SELECT *
                FROM chain.block
              WHERE rollbackToBlock IS NOT NULL AND id = $2 :: bytea AND slotNo = $1 :: bigint
              UNION
              SELECT block.*
                FROM chain.block AS block
                JOIN rollbacks ON block.id = rollbacks.rollbackToBlock AND block.slotNo = rollbacks.rollbackToSlot
            )
          SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
            FROM rollbacks
          WHERE rollbackToSlot IS NULL
        |]

    getTip :: Transaction Api.ChainPoint
    getTip = decodeChainPoint <$> HT.statement ()
      [singletonStatement|
        SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
          FROM chain.block
          WHERE rollbackToSlot IS NULL
          ORDER BY slotNo DESC
          LIMIT 1
      |]

    pointParams :: Maybe (Int64, ByteString)
    pointParams = case point of
      Api.Genesis                                                               -> Nothing
      Api.At (Api.BlockHeader (Api.SlotNo slotNo) (Api.BlockHeaderHash hash) _) -> Just (fromIntegral slotNo, hash)

data PerformMoveResult err result
  = MoveWait
  | MoveAbort err
  | MoveArrive Api.BlockHeader result

performMove :: Api.Move err result -> Api.ChainPoint -> Transaction (PerformMoveResult err result)
performMove = \case
  Api.Fork left right           -> performFork left right
  Api.AdvanceSlots slots        -> performAdvanceSlots slots
  Api.AdvanceBlocks blocks      -> performAdvanceBlocks blocks
  Api.FindConsumingTx txOutRef  -> performFindConsumingTx txOutRef
  Api.FindTx txId wait          -> performFindTx txId wait
  Api.Intersect points          -> performIntersect points
  Api.FindConsumingTxs txOutRef -> performFindConsumingTxs txOutRef

performFork :: Api.Move err1 result1 -> Api.Move err2 result2 -> Api.ChainPoint -> Transaction (PerformMoveResult (These err1 err2) (These result1 result2))
performFork left right point = do
  leftMoveResult <- performMove left point
  rightMoveResult <- performMove right point
  let
    alignResults leftHeader leftResult rightHeader rightResult = case compare leftHeader rightHeader of
      EQ -> MoveArrive leftHeader $ These leftResult rightResult
      LT -> MoveArrive leftHeader $ This leftResult
      GT -> MoveArrive rightHeader $ That rightResult
  pure case (leftMoveResult, rightMoveResult) of
    (MoveAbort leftErr, MoveAbort rightErr)                                -> MoveAbort $ These leftErr rightErr
    (MoveAbort leftErr, _)                                                 -> MoveAbort $ This leftErr
    (_, MoveAbort rightErr)                                                -> MoveAbort $ That rightErr
    (MoveArrive leftHeader leftResult, MoveArrive rightHeader rightResult) -> alignResults leftHeader leftResult rightHeader rightResult
    (MoveArrive leftHeader leftResult, MoveWait)                           -> MoveArrive leftHeader $ This leftResult
    (MoveWait, MoveArrive rightHeader rightResult)                         -> MoveArrive rightHeader $ That rightResult
    _                                                                      -> MoveWait

performAdvanceSlots :: Natural -> Api.ChainPoint -> Transaction (PerformMoveResult Void ())
performAdvanceSlots slots point = do
  decodeAdvance <$> HT.statement (fromIntegral slots + pointSlot point)
    [maybeStatement|
      SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
        FROM chain.block
      WHERE slotNo >= $1 :: bigint
        AND rollbackToBlock IS NULL
      ORDER BY slotNo
      LIMIT 1
    |]

performAdvanceBlocks :: Natural -> Api.ChainPoint -> Transaction (PerformMoveResult Void ())
performAdvanceBlocks blocks point = do
  decodeAdvance <$> HT.statement (pointSlot point, fromIntegral blocks)
    [maybeStatement|
      SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
        FROM chain.block
      WHERE slotNo >= $1 :: bigint
        AND rollbackToBlock IS NULL
      ORDER BY slotNo
      LIMIT 1
      OFFSET $2 :: int
    |]

performFindConsumingTx :: Api.TxOutRef -> Api.ChainPoint -> Transaction (PerformMoveResult Api.UTxOError Api.Transaction)
performFindConsumingTx Api.TxOutRef{..} point = do
  initialResult <- HT.statement (pointSlot point, Api.unTxId txId, fromIntegral txIx) $
    [foldStatement|
      SELECT block.slotNo :: bigint?
           , block.id :: bytea?
           , block.blockNo :: bigint?
           , tx.id :: bytea?
           , tx.validityLowerBound :: bigint?
           , tx.validityUpperBound :: bigint?
           , tx.metadataKey1564 :: bytea?
           , asset.policyId :: bytea?
           , asset.name :: bytea?
           , assetMint.quantity :: bigint?
        FROM chain.txOut AS txOut
        LEFT JOIN chain.txIn      AS txIn      ON txIn.txOutId = txOut.txId AND txIn.txOutIx = txOut.txIx
        LEFT JOIN chain.tx        AS tx        ON tx.id = txIn.txInId AND tx.slotNo = txIn.slotNo
        LEFT JOIN chain.block     AS block     ON block.id = tx.blockId AND block.slotNo = tx.slotNo
        LEFT JOIN chain.assetMint AS assetMint ON assetMint.txId = tx.id AND assetMint.slotNo = tx.slotNo
        LEFT JOIN chain.asset     AS asset     ON asset.id = assetMint.assetId
        WHERE txOut.slotNo <= $1 :: bigint
          AND block.rollbackToBlock IS NULL
          AND txOut.txId = $2 :: bytea
          AND txOut.txIx = $3 :: smallint
    |] foldTx
  case initialResult of
    MoveWait -> pure MoveWait
    MoveAbort err -> pure $ MoveAbort err
    MoveArrive header@Api.BlockHeader{..} tx@Api.Transaction{txId = spendingTxId} ->  do
      txIns <- queryTxIns slotNo spendingTxId
      txOuts <- queryTxOuts slotNo spendingTxId
      pure $ MoveArrive header tx { Api.inputs = txIns, Api.outputs = txOuts }
  where
    foldTx :: Fold ReadTxRow (PerformMoveResult Api.UTxOError Api.Transaction)
    foldTx = Fold foldTx' (MoveAbort Api.UTxONotFound) id

    foldTx'
      :: PerformMoveResult Api.UTxOError Api.Transaction
      -> ReadTxRow
      -> PerformMoveResult Api.UTxOError Api.Transaction
    foldTx' (MoveAbort (Api.UTxOSpent spendingTxId)) _ = MoveAbort $ Api.UTxOSpent spendingTxId
    foldTx' MoveWait _                                 = MoveWait
    foldTx' (MoveAbort Api.UTxONotFound) row           = readFirstTxRow row
    foldTx' (MoveArrive header tx) row                 = MoveArrive header $ mergeTxRow tx row

    readFirstTxRow :: ReadTxRow -> PerformMoveResult Api.UTxOError Api.Transaction
    readFirstTxRow
      ( Just slotNo
      , Just hash
      , Just blockNo
      , Just spendingTxId
      , validityLowerBound
      , validityUpperBound
      , _
      , policyId
      , tokenName
      , quantity
      ) | slotNo <= pointSlot point = MoveAbort $ Api.UTxOSpent $ Api.TxId spendingTxId
        | otherwise                 = MoveArrive (decodeBlockHeader (slotNo, hash, blockNo)) Api.Transaction
          { txId = Api.TxId spendingTxId
          , validityRange = case (validityLowerBound, validityUpperBound) of
              (Nothing, Nothing) -> Api.Unbounded
              (Just lb, Nothing) -> Api.MinBound $ decodeSlotNo lb
              (Nothing, Just ub) -> Api.MaxBound $ decodeSlotNo ub
              (Just lb, Just ub) -> Api.MinMaxBound (decodeSlotNo lb) $ decodeSlotNo ub
          , metadata = Nothing
          , inputs = Set.empty
          , outputs = []
          , mintedTokens = decodeTokens policyId tokenName quantity
          }
    readFirstTxRow _ = MoveWait

performFindConsumingTxs
  :: Set Api.TxOutRef
  -> Api.ChainPoint
  -> Transaction (PerformMoveResult (Map Api.TxOutRef Api.UTxOError) (Map Api.TxOutRef Api.Transaction))
performFindConsumingTxs utxos point = do
  -- TODO consider refactoring this to perform a bulk query rather than querying in a loop
  -- For each requested Tx, perform a FindConsumingTx query
  results <- traverse (flip performFindConsumingTx point) $ Map.fromSet id utxos

  -- partition the results obtained from the individual queries
  let (aborts, Sum waits, arrives) = partitionResults results

  -- limit the found transactions to the ones from the earliest block in the
  -- future.
  let txsFromEarliestBlock = Map.foldrWithKey foldEarliestBlockTxs Nothing arrives

  -- did any of the queries indicate that a result should be awaited?
  let encounteredWaits = waits > 0

  pure case (not $ Map.null aborts, txsFromEarliestBlock, encounteredWaits) of
    -- There were no errors, no consuming Txs were found, and there were UTxOs to
    -- wait for. In this case, the client should wait for results.
    (False, Nothing, True)              -> MoveWait

    -- There were no errors, no consuming Txs were found, and no UTxOs to wait
    -- for. In this case, the client provided an empty set of UTxOs and this is
    -- an error.
    (False, Nothing, False)             -> MoveAbort mempty

    -- There were no errors and some consuming Txs were found. move the client
    -- to the block where the Txs reside and return them.
    (False, Just (blockHeader, txs), _) -> MoveArrive blockHeader txs

    -- There were some errors. Pass these back.
    (True, _, _)                        -> MoveAbort aborts

  where
    foldEarliestBlockTxs utxo (header, tx) Nothing = Just (header, Map.singleton utxo tx)
    foldEarliestBlockTxs utxo (header', tx) (Just (header, txs)) = Just case compare header' header of
      LT -> (header', Map.singleton utxo tx)
      EQ -> (header, Map.insert utxo tx txs)
      GT -> (header, txs)

    partitionResults = Map.foldMapWithKey \utxo -> \case
      MoveArrive header tx -> (mempty, mempty, Map.singleton utxo (header, tx))
      MoveWait             -> (mempty, 1 :: Sum Int, mempty)
      MoveAbort err        -> (Map.singleton utxo err, mempty, mempty)

performFindTx :: Api.TxId -> Bool -> Api.ChainPoint -> Transaction (PerformMoveResult Api.TxError Api.Transaction)
performFindTx txId wait point = do
  initialResult <- HT.statement (Api.unTxId txId) $
    [foldStatement|
      SELECT block.slotNo :: bigint?
           , block.id :: bytea?
           , block.blockNo :: bigint?
           , tx.id :: bytea?
           , tx.validityLowerBound :: bigint?
           , tx.validityUpperBound :: bigint?
           , tx.metadataKey1564 :: bytea?
           , asset.policyId :: bytea?
           , asset.name :: bytea?
           , assetMint.quantity :: bigint?
        FROM chain.tx             AS tx
        JOIN chain.block          AS block     ON block.id = tx.blockId AND block.slotNo = tx.slotNo
        LEFT JOIN chain.assetMint AS assetMint ON assetMint.txId = tx.id AND assetMint.slotNo = tx.slotNo
        LEFT JOIN chain.asset     AS asset     ON asset.id = assetMint.assetId
        WHERE block.rollbackToBlock IS NULL
          AND tx.id = $1 :: bytea
    |] foldTx
  case initialResult of
    MoveWait
      | wait -> pure MoveWait
      | otherwise -> pure $ MoveAbort Api.TxNotFound
    MoveAbort err -> pure $ MoveAbort err
    MoveArrive header@Api.BlockHeader{..} tx ->  do
      txIns <- queryTxIns slotNo txId
      txOuts <- queryTxOuts slotNo txId
      pure $ MoveArrive header tx { Api.inputs = txIns, Api.outputs = txOuts }
  where
    foldTx :: Fold ReadTxRow (PerformMoveResult Api.TxError Api.Transaction)
    foldTx = Fold foldTx' MoveWait id

    foldTx'
      :: PerformMoveResult Api.TxError Api.Transaction
      -> ReadTxRow
      -> PerformMoveResult Api.TxError Api.Transaction
    foldTx' MoveWait row      = readFirstTxRow row
    foldTx' (MoveArrive header tx) row          = MoveArrive header $ mergeTxRow tx row
    foldTx' x _ = x

    readFirstTxRow :: ReadTxRow -> PerformMoveResult Api.TxError Api.Transaction
    readFirstTxRow
      ( Just slotNo
      , Just hash
      , Just blockNo
      , Just spendingTxId
      , validityLowerBound
      , validityUpperBound
      , _
      , policyId
      , tokenName
      , quantity
      ) | slotNo <= pointSlot point = MoveAbort $ Api.TxInPast $ decodeBlockHeader (slotNo, hash, blockNo)
        | otherwise                 = MoveArrive (decodeBlockHeader (slotNo, hash, blockNo)) Api.Transaction
          { txId = Api.TxId spendingTxId
          , validityRange = case (validityLowerBound, validityUpperBound) of
              (Nothing, Nothing) -> Api.Unbounded
              (Just lb, Nothing) -> Api.MinBound $ decodeSlotNo lb
              (Nothing, Just ub) -> Api.MaxBound $ decodeSlotNo ub
              (Just lb, Just ub) -> Api.MinMaxBound (decodeSlotNo lb) $ decodeSlotNo ub
          , metadata = Nothing
          , inputs = Set.empty
          , outputs = []
          , mintedTokens = decodeTokens policyId tokenName quantity
          }
    readFirstTxRow _ = MoveWait

mergeTxRow :: Api.Transaction -> ReadTxRow -> Api.Transaction
mergeTxRow tx@Api.Transaction{mintedTokens} (_, _, _, _, _, _, _, policyId, tokenName, quantity) = tx
  { Api.mintedTokens = mintedTokens <> decodeTokens policyId tokenName quantity
  }

queryTxIns :: Api.SlotNo -> Api.TxId -> Transaction (Set.Set Api.TransactionInput)
queryTxIns slotNo txInId = HT.statement (Api.unTxId txInId, fromIntegral slotNo) $
  [foldStatement|
    SELECT txIn.txOutId :: bytea
         , txIn.txOutIx :: smallint
         , txOut.address :: bytea
         , txOut.datumBytes :: bytea?
         , txIn.redeemerDatumBytes :: bytea?
      FROM chain.txIn  as txIn
      JOIN chain.txOut as txOut ON txOut.txId = txIn.txOutId AND txOut.txIx = txIn.txOutIx
     WHERE txIn.txInId = $1 :: bytea AND txIn.slotNo = $2 :: bigint
  |] (Fold.foldMap (Set.singleton . decodeTxIn) id)
  where
    decodeTxIn :: ReadTxInRow -> Api.TransactionInput
    decodeTxIn (txId, txIx, address, datumBytes, redeemerDatumBytes) = Api.TransactionInput
      { txId = Api.TxId txId
      , txIx = fromIntegral txIx
      , address = Api.Address address
      , datumBytes = Api.fromPlutusData . toPlutusData . unsafeDeserialize' <$> datumBytes
      , redeemer = Api.Redeemer . Api.fromPlutusData . toPlutusData . unsafeDeserialize' <$> redeemerDatumBytes
      }

queryTxOuts :: Api.SlotNo -> Api.TxId -> Transaction [Api.TransactionOutput]
queryTxOuts slotNo txId = HT.statement (Api.unTxId txId, fromIntegral slotNo) $
  [foldStatement|
    SELECT txOut.txIx :: smallint
         , txOut.address :: bytea
         , txOut.lovelace :: bigint
         , txOut.datumHash :: bytea?
         , txOut.datumBytes :: bytea?
         , asset.policyId :: bytea?
         , asset.name :: bytea?
         , assetOut.quantity :: bigint?
      FROM chain.txOut         AS txOut
      LEFT JOIN chain.assetOut AS assetOut ON assetOut.txOutId = txOut.txId AND assetOut.txOutIx = txOut.txIx
      LEFT JOIN chain.asset    AS asset    ON asset.id = assetOut.assetId
     WHERE txOut.txId = $1 :: bytea AND txOut.slotNo = $2 :: bigint
     ORDER BY txIx
  |] (Fold foldRow IntMap.empty (fmap snd . IntMap.toAscList))
  where
    foldRow :: IntMap Api.TransactionOutput -> ReadTxOutRow -> IntMap Api.TransactionOutput
    foldRow acc (txIx, address, lovelace, datumHash, datumBytes, policyId, tokenName, quantity) =
      IntMap.alter (Just . maybe newTxOut mergeTxOut) (fromIntegral txIx) acc
      where
        newTxOut = Api.TransactionOutput
          { address = Api.Address address
          , assets = Api.Assets
              { ada = Api.Lovelace $ fromIntegral lovelace
              , tokens = decodeTokens policyId tokenName quantity
              }
          , datumHash = Api.DatumHash <$> datumHash
          , datum = Api.fromPlutusData . toPlutusData . unsafeDeserialize' <$> datumBytes
          }

        mergeTxOut txOut@Api.TransactionOutput{assets = assets@Api.Assets{..}} = txOut
          { Api.assets = assets
              { Api.tokens = tokens <> decodeTokens policyId tokenName quantity
              }
          }

decodeTokens :: Maybe ByteString -> Maybe ByteString -> Maybe Int64 -> Api.Tokens
decodeTokens (Just policyId) (Just tokenName) (Just quantity) =
  Api.Tokens $ Map.singleton
    (Api.AssetId (Api.PolicyId policyId) (Api.TokenName tokenName))
    (Api.Quantity $ fromIntegral quantity)
decodeTokens _ _ _ = mempty

type ReadTxRow =
  ( Maybe Int64      -- Tx Block's SlotNo
  , Maybe ByteString -- Tx Block's BlockHeaderHash
  , Maybe Int64      -- Tx Block's BlockNo

  , Maybe ByteString -- Tx id
  , Maybe Int64      -- Tx validityLowerBound
  , Maybe Int64      -- Tx validityUpperBound
  , Maybe ByteString -- Tx metadataKey1564

  , Maybe ByteString -- Tx minted Token's PolicyId
  , Maybe ByteString -- Tx minted Token's TokenName
  , Maybe Int64      -- Tx minted Token's Quantity
  )

type ReadTxInRow =
  ( ByteString       -- TxIn's TxId
  , Int16            -- TxIn's TxIx
  , ByteString       -- TxIn's Address
  , Maybe ByteString -- TxIn's datumBytes
  , Maybe ByteString -- TxIn's redeemerDatumBytes
  )

type ReadTxOutRow =
  ( Int16            -- TxOut's TxIx
  , ByteString       -- TxOut's Address
  , Int64            -- TxOut's Lovelace
  , Maybe ByteString -- TxOut's DatumHash
  , Maybe ByteString -- TxOut's datumBytes
  , Maybe ByteString -- TxOut's Token's PolicyId
  , Maybe ByteString -- TxOut's Token's TokenName
  , Maybe Int64      -- TxOut's Token's Quantity
  )

performIntersect :: [Api.BlockHeader] -> Api.ChainPoint -> Transaction (PerformMoveResult Api.IntersectError ())
performIntersect points point = do
    let
      params =
        ( V.fromList $ (\Api.BlockHeader{..} -> fromIntegral slotNo) <$> points
        , V.fromList $ (\Api.BlockHeader{..} -> Api.unBlockHeaderHash headerHash) <$> points
        , pointSlot point
        )
    decodeAdvance <$> HT.statement params
      [maybeStatement|
        WITH points (slotNo, id) AS
          ( SELECT * FROM UNNEST ($1 :: bigint[], $2 :: bytea[])
          )
        SELECT block.slotNo :: bigint, block.id :: bytea, block.blockNo :: bigint
          FROM chain.block AS block
          JOIN points AS point USING (slotNo, id)
          WHERE rollbackToBlock IS NULL
            AND block.slotNo > $3 :: bigint
          ORDER BY slotNo DESC
          LIMIT 1
      |]

-- CommitRollback

commitRollback :: GenesisBlock -> CommitRollback Transaction
commitRollback genesisBlock = CommitRollback \case
  ChainPointAtGenesis -> do
    -- truncate all tables
    HT.sql $ BS.intercalate "\n"
      [ "TRUNCATE TABLE chain.tx;"
      , "TRUNCATE TABLE chain.txOut;"
      , "TRUNCATE TABLE chain.txIn;"
      , "TRUNCATE TABLE chain.asset RESTART IDENTITY CASCADE;"
      , "TRUNCATE TABLE chain.block;"
      ]
    -- re-add the genesis block (faster than excluding the info using DELETE FROM ... WHERE ...)
    runCommitGenesisBlock commitGenesisBlock genesisBlock
  ChainPoint slotNo hash -> HT.statement (slotNoToParam slotNo, headerHashToParam hash)
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
--
-- NOTE: If this query ever gets refactored, it is extremely important that all
-- writes are atomic at the block level (i.e. it should be impossible for a
-- block to be partially saved). This is because the server may be shut down at
-- any moment, and the connection to the database will be severed.
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
          ( SELECT * FROM UNNEST ($11 :: bytea[], $12 :: smallint[], $13 :: bigint[], $14 :: bytea[], $15 :: bigint[], $16 :: bytea?[], $17 :: bytea?[], $18 :: boolean[])
          )
        , newTxOuts AS
          ( INSERT INTO chain.txOut (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral)
            SELECT txOut.txId, txOut.txIx, txOut.slotNo, txOut.address, txOut.lovelace, txOut.datumHash, txOut.datumBytes, txOut.isCollateral
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

txIxToParam :: TxIx -> Int16
txIxToParam (TxIx txIx) = fromIntegral txIx

decodeBlockHeader :: (Int64, ByteString, Int64) -> Api.BlockHeader
decodeBlockHeader (slotNo, hash, blockNo) = Api.BlockHeader (decodeSlotNo slotNo) (Api.BlockHeaderHash hash) (decodeBlockNo blockNo)

decodeChainPoint :: (Int64, ByteString, Int64) -> Api.ChainPoint
decodeChainPoint (-1, _, _) = Api.Genesis
decodeChainPoint row        = Api.At $ decodeBlockHeader row

decodeSlotNo :: Int64 -> Api.SlotNo
decodeSlotNo = Api.SlotNo . fromIntegral

decodeBlockNo :: Int64 -> Api.BlockNo
decodeBlockNo = Api.BlockNo . fromIntegral

pointSlot :: Api.ChainPoint -> Int64
pointSlot Api.Genesis                  = -1
pointSlot (Api.At Api.BlockHeader{..}) = fromIntegral slotNo

decodeAdvance :: Maybe (Int64, ByteString, Int64) -> PerformMoveResult err ()
decodeAdvance Nothing    = MoveWait
decodeAdvance (Just row) = MoveArrive (decodeBlockHeader row) ()
