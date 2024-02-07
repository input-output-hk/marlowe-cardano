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

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL (
  QueryField (..),
  QuerySelector (..),
  databaseQueries,
) where

import Cardano.Api (BabbageEraOnwards (..), ShelleyBasedEra (..))
import Cardano.Api.Shelley (fromShelleyBasedScript)
import qualified Cardano.Api.Shelley as C
import Cardano.Binary (unsafeDeserialize')
import Cardano.Ledger.Binary (DecCBOR (decCBOR), decodeFullAnnotator, shelleyProtVer)
import Control.Applicative ((<|>))
import Control.Arrow (Arrow (..), (***))
import Control.Foldl (Fold (Fold))
import qualified Control.Foldl as Fold
import Control.Monad (guard)
import Control.Monad.Event.Class (MonadInjectEvent, withEventFields)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.Int (Int16, Int64)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (Sum (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Set.NonEmpty (NESet)
import qualified Data.Set.NonEmpty as NESet
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void (Void, absurd)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.TH (foldStatement, maybeStatement, singletonStatement, vectorStatement)
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as TS
import Language.Marlowe.Runtime.Cardano.Feature (hush)
import Language.Marlowe.Runtime.ChainSync.Api
import Language.Marlowe.Runtime.ChainSync.Database (
  Collect (..),
  CollectResult (..),
  DatabaseQueries (DatabaseQueries),
  GetTip (..),
  GetUTxOs (..),
  MoveClient (..),
  MoveResult (..),
  Scan (..),
  hoistCollect,
  hoistGetScripts,
  hoistGetTip,
  hoistGetUTxOs,
  hoistMoveClient,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Database as DB
import qualified Language.Marlowe.Runtime.ChainSync.Database as Database
import Numeric.Natural (Natural)
import Observe.Event (addField)
import UnliftIO (throwIO)
import Prelude hiding (init)

data QuerySelector f where
  Query :: Text -> QuerySelector QueryField

data QueryField
  = SqlStatement ByteString
  | Parameters [Text]
  | QueryName Text

-- | PostgreSQL implementation for the marlowe-chain-sync database queries.
databaseQueries
  :: forall r s m. (MonadInjectEvent r QuerySelector s m, MonadIO m) => Pool -> C.NetworkId -> DatabaseQueries m
databaseQueries pool networkId =
  DatabaseQueries
    (hoistGetUTxOs (transact "getUTxOs") getUTxOs)
    (hoistGetScripts (transact "getScripts") getScripts)
    (hoistGetTip (transact "getTip") getTip)
    (hoistMoveClient (transact "moveClient") $ moveClient networkId)
    (Scan $ fmap (pure . hoistCollect (transact "collect")) . mkCollect networkId)
  where
    transact :: Text -> HT.Transaction a -> m a
    transact queryName m = withEventFields (Query queryName) [QueryName queryName] \ev -> do
      result <- liftIO $ Pool.use pool $ TS.transaction TS.Serializable TS.Read m
      case result of
        Left ex -> do
          case ex of
            (Pool.SessionUsageError (Session.QueryError sql params _)) -> do
              addField ev $ SqlStatement sql
              addField ev $ Parameters params
            _ -> pure ()
          throwIO ex
        Right a -> pure a

-- GetScripts

getScripts :: Database.GetScripts HT.Transaction
getScripts = Database.GetScripts go
  where
    go :: forall era. C.BabbageEraOnwards era -> Set ScriptHash -> HT.Transaction (Map ScriptHash (C.ScriptInEra era))
    go era scripts
      | Set.null scripts = pure mempty
      | otherwise =
          Map.fromDistinctAscList . fmap mapRow . V.toList
            <$> HT.statement
              (params scripts)
              [vectorStatement|
                  SELECT id :: bytea, bytes :: bytea
                  FROM chain.script
                  WHERE id = ANY($1 :: bytea[])
                  ORDER BY id
                |]
      where
        params = V.fromList . fmap unScriptHash . Set.toList

        mapRow :: (ByteString, ByteString) -> (ScriptHash, C.ScriptInEra era)
        mapRow = case era of
          BabbageEraOnwardsBabbage -> \(scriptHash, scriptBytes) ->
            ( ScriptHash scriptHash
            , fromShelleyBasedScript ShelleyBasedEraBabbage $
                either (error . show) id $
                  decodeFullAnnotator shelleyProtVer "Script" decCBOR $
                    LBS.fromStrict scriptBytes
            )
          BabbageEraOnwardsConway -> \(scriptHash, scriptBytes) ->
            ( ScriptHash scriptHash
            , fromShelleyBasedScript ShelleyBasedEraConway $
                either (error . show) id $
                  decodeFullAnnotator shelleyProtVer "Script" decCBOR $
                    LBS.fromStrict scriptBytes
            )

-- Scan

mkCollect :: C.NetworkId -> ChainPoint -> Move err result -> Collect err result HT.Transaction
mkCollect networkId point move = Collect \batchSize -> do
  tip <- runGetTip getTip
  getRollbackPoint point >>= \case
    Nothing ->
      performCollect networkId batchSize tip move point >>= \case
        CollectAbort err -> pure $ CollectReject err tip
        Collected results -> pure $ NextBlocks results tip $ mkCollect networkId (At $ fst $ NE.last results) move
        CollectWait' -> pure $ CollectWait tip
    Just rollbackPoint -> pure $ CollectRollBack rollbackPoint tip

data PerformCollectResult err result
  = CollectWait'
  | CollectAbort err
  | Collected (NonEmpty (BlockHeader, result))

performCollect
  :: C.NetworkId
  -> Natural
  -> ChainPoint
  -> Move err result
  -> ChainPoint
  -> HT.Transaction (PerformCollectResult err result)
performCollect networkId batchSize tip = \case
  AdvanceBlocks blocks -> repeatMove [] batchSize $ performAdvanceBlocks blocks
  FindTx txId wait -> collectOne . performFindTx txId wait
  Intersect points -> collectOne . performIntersect points
  FindConsumingTxs txOutRef -> collectOne . performFindConsumingTxs txOutRef
  FindTxsFor credentials -> collectTxsFor networkId batchSize credentials
  AdvanceToTip -> \point -> pure case tip of
    Genesis -> CollectWait'
    At tipBlock
      | point == tip -> CollectWait'
      | otherwise -> Collected $ pure (tipBlock, ())

collectOne :: HT.Transaction (PerformMoveResult err a) -> HT.Transaction (PerformCollectResult err a)
collectOne query =
  query <&> \case
    MoveWait -> CollectWait'
    MoveAbort err -> CollectAbort err
    MoveArrive point result -> Collected $ pure (point, result)

repeatMove
  :: [(BlockHeader, a)]
  -> Natural
  -> (ChainPoint -> HT.Transaction (PerformMoveResult Void a))
  -> ChainPoint
  -> HT.Transaction (PerformCollectResult Void a)
repeatMove [] 0 _ _ = pure CollectWait'
repeatMove acc 0 _ _ = pure $ Collected $ NE.fromList $ reverse acc
repeatMove acc n query point =
  query point >>= \case
    MoveWait -> pure CollectWait'
    MoveAbort err -> absurd err
    MoveArrive point' result -> repeatMove ((point', result) : acc) (pred n) query $ At point'

collectTxsFor
  :: C.NetworkId
  -> Natural
  -> NESet Credential
  -> ChainPoint
  -> HT.Transaction (PerformCollectResult Void (Set Transaction))
collectTxsFor networkId batchSize credentials fromPoint =
  maybe CollectWait' Collected <$> runMaybeT do
    blocks <- lift loadTxIds
    let txIds = fold blocks
    guard $ not $ Set.null txIds
    lift do
      txs <- loadTxs txIds
      txIns <- queryTxInsBulk txIds
      txOuts <- queryTxOutsBulk txIds
      pure $ assembleResults blocks txs txIns txOuts
  where
    loadTxIds :: HT.Transaction (Map BlockHeader (Set TxId))
    loadTxIds = do
      -- Fetching the from and to txs and block headers separately is significantly more efficient than
      -- doing everything in a single query using CTEs.
      fromTxs <-
        Map.fromDistinctAscList . V.toList
          <$> HT.statement
            params
            [vectorStatement|
                WITH credentials (addressHeader,  addressPaymentCredential) as
                  ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[])
                  )
                SELECT
                  slotNo :: bigint,
                  ARRAY_AGG(txId) :: bytea[]
                FROM chain.txOut
                NATURAL JOIN credentials
                WHERE slotNo > $3 :: bigint
                GROUP BY slotNo
                ORDER BY slotNo
                LIMIT $4 :: int
            |]
      toTxs <-
        Map.fromDistinctAscList . V.toList
          <$> HT.statement
            params
            [vectorStatement|
                WITH credentials (addressHeader,  addressPaymentCredential) as
                  ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[])
                  )
                SELECT
                  txIn.slotNo :: bigint,
                  ARRAY_AGG(txIn.txInId) :: bytea[]
                FROM chain.txIn
                JOIN chain.txOut
                  ON txOut.txId = txIn.txOutId
                  AND txOut.txIx = txIn.txOutIx
                NATURAL JOIN credentials
                WHERE txIn.slotNo > $3 :: bigint
                GROUP BY txIn.slotNo
                ORDER BY txIn.slotNo
                LIMIT $4 :: int
            |]
      let mergedTxs =
            Map.fromDistinctAscList
              . take (fromIntegral batchSize)
              . Map.toAscList
              . fmap (Set.fromList . V.toList . fmap TxId)
              $ Map.unionWith (<>) fromTxs toTxs
      blockHeaders <-
        Map.fromDistinctAscList . fmap decodeBlockHeaderRow . V.toList
          <$> HT.statement
            (V.fromList $ Map.keys mergedTxs)
            [vectorStatement|
                WITH slots (slotNo) as
                  ( SELECT * FROM UNNEST ($1 :: bigint[])
                  )
                SELECT
                  slotNo :: bigint,
                  id :: bytea,
                  blockNo :: bigint
                FROM chain.block
                NATURAL JOIN slots
                WHERE rollbackToBlock IS NULL
                ORDER BY slotNo
            |]
      pure $ Map.mapKeysMonotonic (\slot -> fromJust $ Map.lookup slot blockHeaders) mergedTxs
      where
        params =
          ( V.fromList $ fst <$> addressParts
          , V.fromList $ snd <$> addressParts
          , pointSlot fromPoint
          , fromIntegral batchSize
          )

        addressParts =
          Set.toList (NESet.toSet credentials) >>= \case
            PaymentKeyCredential pkh ->
              (,unPaymentKeyHash pkh) . BS.pack . pure
                <$> if networkId == C.Mainnet then [0x01, 0x21, 0x41, 0x61] else [0x00, 0x20, 0x40, 0x60]
            ScriptCredential sh ->
              (,unScriptHash sh) . BS.pack . pure
                <$> if networkId == C.Mainnet then [0x11, 0x31, 0x51, 0x71] else [0x10, 0x30, 0x50, 0x70]

        decodeBlockHeaderRow :: (Int64, ByteString, Int64) -> (Int64, BlockHeader)
        decodeBlockHeaderRow (slotNo, blockHeaderHash, blockNo) =
          ( slotNo
          , decodeBlockHeader (slotNo, blockHeaderHash, blockNo)
          )

    loadTxs :: Set TxId -> HT.Transaction (Map TxId Transaction)
    loadTxs txIds =
      Map.fromDistinctAscList . fmap mapRow . V.toList
        <$> HT.statement
          params
          [vectorStatement|
            SELECT
              tx.id :: bytea,
              (ARRAY_AGG(tx.validityLowerBound))[1] :: bigint?,
              (ARRAY_AGG(tx.validityUpperBound))[1] :: bigint?,
              (ARRAY_AGG(tx.metadata))[1] :: bytea?,
              ARRAY_REMOVE(ARRAY_AGG(assetMint.policyId), NULL) :: bytea[],
              ARRAY_REMOVE(ARRAY_AGG(assetMint.name), NULL) :: bytea[],
              ARRAY_REMOVE(ARRAY_AGG(assetMint.quantity), NULL) :: bigint[]
            FROM chain.tx
            LEFT JOIN chain.assetMint
              ON assetMint.txId = tx.id
            WHERE tx.id = ANY($1 :: bytea[])
            GROUP BY tx.id
            ORDER BY tx.id
          |]
      where
        params = V.fromList $ fmap unTxId $ Set.toList txIds

        mapRow
          :: (ByteString, Maybe Int64, Maybe Int64, Maybe ByteString, Vector ByteString, Vector ByteString, Vector Int64)
          -> (TxId, Transaction)
        mapRow (txId, invalidBefore, invalidHereafter, metadata, policyIds, tokenNames, quantities) =
          ( TxId txId
          , Transaction
              { txId = TxId txId
              , validityRange = case (invalidBefore, invalidHereafter) of
                  (Nothing, Nothing) -> Unbounded
                  (Just lb, Nothing) -> MinBound $ decodeSlotNo lb
                  (Nothing, Just ub) -> MaxBound $ decodeSlotNo ub
                  (Just lb, Just ub) -> MinMaxBound (decodeSlotNo lb) $ decodeSlotNo ub
              , metadata = maybe mempty fromCardanoTxMetadata $ decodeMetadata =<< metadata
              , inputs = Set.empty
              , outputs = []
              , mintedTokens =
                  Tokens $
                    Map.fromList $
                      V.toList $
                        V.zipWith3 mkToken policyIds tokenNames quantities
              }
          )

    mkToken :: ByteString -> ByteString -> Int64 -> (AssetId, Quantity)
    mkToken policyId tokenName quantity =
      ( AssetId (PolicyId policyId) (TokenName tokenName)
      , Quantity $ fromIntegral quantity
      )

    assembleResults
      :: Map BlockHeader (Set TxId)
      -> Map TxId Transaction
      -> Map TxId (Set TransactionInput)
      -> Map TxId [TransactionOutput]
      -> NonEmpty (BlockHeader, Set Transaction)
    assembleResults txIds txs txIns txOuts =
      NE.fromList $
        Map.toList $
          Set.fromList . Map.elems . Map.restrictKeys mergedTxs <$> txIds
      where
        mergedTxs = Map.intersectionWith ($) (Map.intersectionWith mergeTx txs txIns) txOuts

        mergeTx :: Transaction -> Set TransactionInput -> [TransactionOutput] -> Transaction
        mergeTx Transaction{..} inputs' outputs' = Transaction{inputs = inputs', outputs = outputs', ..}

-- MoveClient

moveClient :: C.NetworkId -> MoveClient HT.Transaction
moveClient networkId = MoveClient $ performMoveWithRollbackCheck networkId

performMoveWithRollbackCheck :: C.NetworkId -> ChainPoint -> Move err result -> HT.Transaction (MoveResult err result)
performMoveWithRollbackCheck networkId point move = do
  tip <- runGetTip getTip
  getRollbackPoint point >>= \case
    Nothing ->
      performMove networkId tip move point >>= \case
        MoveAbort err -> pure $ Reject err tip
        MoveArrive point' result -> pure $ RollForward result point' tip
        MoveWait -> pure $ Wait tip
    Just rollbackPoint -> pure $ RollBack rollbackPoint tip

getRollbackPoint :: ChainPoint -> HT.Transaction (Maybe ChainPoint)
getRollbackPoint point = case pointParams of
  Nothing -> pure Nothing
  Just pointParams' ->
    fmap decodeChainPoint
      <$> HT.statement
        pointParams'
        [maybeStatement|
          WITH RECURSIVE rollbacks AS
            ( SELECT *
                FROM chain.block
              WHERE rollbackToBlock IS NOT NULL
                AND id = $2 :: bytea
                AND slotNo = $1 :: bigint
                AND NOT EXISTS
                  ( SELECT *
                      FROM chain.block
                    WHERE rollbackToBlock IS NULL
                      AND id = $2 :: bytea
                      AND slotNo = $1 :: bigint
                  )
              UNION
              SELECT block.*
                FROM chain.block AS block
                JOIN rollbacks ON block.id = rollbacks.rollbackToBlock AND block.slotNo = rollbacks.rollbackToSlot
            )
          SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
            FROM rollbacks
          WHERE rollbackToSlot IS NULL
          ORDER BY slotNo DESC
          LIMIT 1
    |]
  where
    pointParams :: Maybe (Int64, ByteString)
    pointParams = case point of
      Genesis -> Nothing
      At (BlockHeader (SlotNo slotNo) (BlockHeaderHash hash) _) -> Just (fromIntegral slotNo, hash)

data PerformMoveResult err result
  = MoveWait
  | MoveAbort err
  | MoveArrive BlockHeader result

performMove
  :: C.NetworkId -> ChainPoint -> Move err result -> ChainPoint -> HT.Transaction (PerformMoveResult err result)
performMove networkId tip = \case
  AdvanceBlocks blocks -> performAdvanceBlocks blocks
  FindTx txId wait -> performFindTx txId wait
  Intersect points -> performIntersect points
  FindConsumingTxs txOutRef -> performFindConsumingTxs txOutRef
  FindTxsFor credentials -> performFindTxsFor networkId credentials
  AdvanceToTip -> \point -> pure case tip of
    Genesis -> MoveWait
    At tipBlock
      | point == tip -> MoveWait
      | otherwise -> MoveArrive tipBlock ()

performAdvanceBlocks :: Natural -> ChainPoint -> HT.Transaction (PerformMoveResult Void ())
performAdvanceBlocks blocks point = do
  decodeAdvance
    <$> HT.statement
      (pointSlot point, fromIntegral blocks)
      [maybeStatement|
      SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
        FROM chain.block
      WHERE slotNo >= $1 :: bigint
        AND rollbackToBlock IS NULL
      ORDER BY slotNo
      LIMIT 1
      OFFSET $2 :: int
    |]

performFindConsumingTx :: TxOutRef -> ChainPoint -> HT.Transaction (PerformMoveResult UTxOError Transaction)
performFindConsumingTx TxOutRef{..} point = do
  initialResult <-
    HT.statement (pointSlot point, unTxId txId, fromIntegral txIx) $
      [foldStatement|
      SELECT block.slotNo :: bigint?
           , block.id :: bytea?
           , block.blockNo :: bigint?
           , tx.id :: bytea?
           , tx.validityLowerBound :: bigint?
           , tx.validityUpperBound :: bigint?
           , tx.metadata :: bytea?
           , assetMint.policyId :: bytea?
           , assetMint.name :: bytea?
           , assetMint.quantity :: bigint?
        FROM chain.txOut AS txOut
        LEFT JOIN chain.txIn      AS txIn      ON txIn.txOutId = txOut.txId AND txIn.txOutIx = txOut.txIx
        LEFT JOIN chain.tx        AS tx        ON tx.id = txIn.txInId AND tx.slotNo = txIn.slotNo
        LEFT JOIN chain.block     AS block     ON block.id = tx.blockId AND block.slotNo = tx.slotNo
        LEFT JOIN chain.assetMint AS assetMint ON assetMint.txId = tx.id AND assetMint.slotNo = tx.slotNo
        WHERE txOut.slotNo <= $1 :: bigint
          AND block.rollbackToBlock IS NULL
          AND txOut.txId = $2 :: bytea
          AND txOut.txIx = $3 :: smallint
    |]
        foldTx
  case initialResult of
    MoveWait -> pure MoveWait
    MoveAbort err -> pure $ MoveAbort err
    MoveArrive header@BlockHeader{..} tx@Transaction{txId = spendingTxId} -> do
      txIns <- queryTxIns slotNo spendingTxId
      txOuts <- queryTxOuts slotNo spendingTxId
      pure $ MoveArrive header tx{inputs = txIns, outputs = txOuts}
  where
    foldTx :: Fold ReadTxRow (PerformMoveResult UTxOError Transaction)
    foldTx = Fold foldTx' (MoveAbort UTxONotFound) id

    foldTx'
      :: PerformMoveResult UTxOError Transaction
      -> ReadTxRow
      -> PerformMoveResult UTxOError Transaction
    foldTx' (MoveAbort (UTxOSpent spendingTxId)) _ = MoveAbort $ UTxOSpent spendingTxId
    foldTx' MoveWait _ = MoveWait
    foldTx' (MoveAbort UTxONotFound) row = readFirstTxRow row
    foldTx' (MoveArrive header tx) row = MoveArrive header $ mergeTxRow tx row

    readFirstTxRow :: ReadTxRow -> PerformMoveResult UTxOError Transaction
    readFirstTxRow
      ( Just slotNo
        , Just hash
        , Just blockNo
        , Just spendingTxId
        , validityLowerBound
        , validityUpperBound
        , mMetadata
        , policyId
        , tokenName
        , quantity
        )
        | slotNo <= pointSlot point = MoveAbort $ UTxOSpent $ TxId spendingTxId
        | otherwise =
            MoveArrive
              (decodeBlockHeader (slotNo, hash, blockNo))
              Transaction
                { txId = TxId spendingTxId
                , validityRange = case (validityLowerBound, validityUpperBound) of
                    (Nothing, Nothing) -> Unbounded
                    (Just lb, Nothing) -> MinBound $ decodeSlotNo lb
                    (Nothing, Just ub) -> MaxBound $ decodeSlotNo ub
                    (Just lb, Just ub) -> MinMaxBound (decodeSlotNo lb) $ decodeSlotNo ub
                , metadata = maybe mempty fromCardanoTxMetadata $ decodeMetadata =<< mMetadata
                , inputs = Set.empty
                , outputs = []
                , mintedTokens = decodeTokens policyId tokenName quantity
                }
    readFirstTxRow _ = MoveWait

performFindConsumingTxs
  :: Set TxOutRef
  -> ChainPoint
  -> HT.Transaction (PerformMoveResult (Map TxOutRef UTxOError) (Map TxOutRef Transaction))
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
    (False, Nothing, True) -> MoveWait
    -- There were no errors, no consuming Txs were found, and no UTxOs to wait
    -- for. In this case, the client provided an empty set of UTxOs and this is
    -- an error.
    (False, Nothing, False) -> MoveAbort mempty
    -- There were no errors and some consuming Txs were found. move the client
    -- to the block where the Txs reside and return them.
    (False, Just (blockHeader, txs), _) -> MoveArrive blockHeader txs
    -- There were some errors. Pass these back.
    (True, _, _) -> MoveAbort aborts
  where
    foldEarliestBlockTxs utxo (header, tx) Nothing = Just (header, Map.singleton utxo tx)
    foldEarliestBlockTxs utxo (header', tx) (Just (header, txs)) = Just case compare header' header of
      LT -> (header', Map.singleton utxo tx)
      EQ -> (header, Map.insert utxo tx txs)
      GT -> (header, txs)

    partitionResults = Map.foldMapWithKey \utxo -> \case
      MoveArrive header tx -> (mempty, mempty, Map.singleton utxo (header, tx))
      MoveWait -> (mempty, 1 :: Sum Int, mempty)
      MoveAbort err -> (Map.singleton utxo err, mempty, mempty)

performFindTx :: TxId -> Bool -> ChainPoint -> HT.Transaction (PerformMoveResult TxError Transaction)
performFindTx txId wait point = do
  initialResult <-
    HT.statement (unTxId txId) $
      [foldStatement|
      SELECT block.slotNo :: bigint?
           , block.id :: bytea?
           , block.blockNo :: bigint?
           , tx.id :: bytea?
           , tx.validityLowerBound :: bigint?
           , tx.validityUpperBound :: bigint?
           , tx.metadata :: bytea?
           , assetMint.policyId :: bytea?
           , assetMint.name :: bytea?
           , assetMint.quantity :: bigint?
        FROM chain.tx             AS tx
        JOIN chain.block          AS block     ON block.id = tx.blockId AND block.slotNo = tx.slotNo
        LEFT JOIN chain.assetMint AS assetMint ON assetMint.txId = tx.id AND assetMint.slotNo = tx.slotNo
        WHERE block.rollbackToBlock IS NULL
          AND tx.id = $1 :: bytea
    |]
        foldTx
  case initialResult of
    MoveWait
      | wait -> pure MoveWait
      | otherwise -> pure $ MoveAbort TxNotFound
    MoveAbort err -> pure $ MoveAbort err
    MoveArrive header@BlockHeader{..} tx -> do
      txIns <- queryTxIns slotNo txId
      txOuts <- queryTxOuts slotNo txId
      pure $ MoveArrive header tx{inputs = txIns, outputs = txOuts}
  where
    foldTx :: Fold ReadTxRow (PerformMoveResult TxError Transaction)
    foldTx = Fold foldTx' MoveWait id

    foldTx'
      :: PerformMoveResult TxError Transaction
      -> ReadTxRow
      -> PerformMoveResult TxError Transaction
    foldTx' MoveWait row = readFirstTxRow row
    foldTx' (MoveArrive header tx) row = MoveArrive header $ mergeTxRow tx row
    foldTx' x _ = x

    readFirstTxRow :: ReadTxRow -> PerformMoveResult TxError Transaction
    readFirstTxRow
      ( Just slotNo
        , Just hash
        , Just blockNo
        , Just spendingTxId
        , validityLowerBound
        , validityUpperBound
        , mMetadata
        , policyId
        , tokenName
        , quantity
        )
        | slotNo <= pointSlot point = MoveAbort $ TxInPast $ decodeBlockHeader (slotNo, hash, blockNo)
        | otherwise =
            MoveArrive
              (decodeBlockHeader (slotNo, hash, blockNo))
              Transaction
                { txId = TxId spendingTxId
                , validityRange = case (validityLowerBound, validityUpperBound) of
                    (Nothing, Nothing) -> Unbounded
                    (Just lb, Nothing) -> MinBound $ decodeSlotNo lb
                    (Nothing, Just ub) -> MaxBound $ decodeSlotNo ub
                    (Just lb, Just ub) -> MinMaxBound (decodeSlotNo lb) $ decodeSlotNo ub
                , metadata = maybe mempty fromCardanoTxMetadata $ decodeMetadata =<< mMetadata
                , inputs = Set.empty
                , outputs = []
                , mintedTokens = decodeTokens policyId tokenName quantity
                }
    readFirstTxRow _ = MoveWait

performFindTxsFor
  :: C.NetworkId -> NESet Credential -> ChainPoint -> HT.Transaction (PerformMoveResult Void (Set Transaction))
performFindTxsFor networkId credentials point = do
  initialResult <-
    HT.statement params $
      [foldStatement|
      WITH credentials (addressHeader,  addressPaymentCredential) as
        ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[])
        )
      , txIds (id, slotNo) AS
        ( SELECT txOut.txId, txOut.slotNo
            FROM chain.txOut as txOut
            JOIN credentials USING (addressHeader, addressPaymentCredential)
           WHERE txOut.slotNo > $3 :: bigint
           UNION
          SELECT txIn.txInId, txIn.slotNo
            FROM chain.txIn as tXIn
            JOIN chain.txOut as txOut ON txOut.txId = txIn.txOutId AND txOut.txIx = txIn.txOutIx
            JOIN credentials USING (addressHeader, addressPaymentCredential)
           WHERE txIn.slotNo > $3 :: bigint
        )
      , nextSlot (slotNo) AS
        ( SELECT MIN(slotNo) FROM txIds
        )
      SELECT block.slotNo :: bigint?
           , block.id :: bytea?
           , block.blockNo :: bigint?
           , tx.id :: bytea?
           , tx.validityLowerBound :: bigint?
           , tx.validityUpperBound :: bigint?
           , tx.metadata :: bytea?
           , assetMint.policyId :: bytea?
           , assetMint.name :: bytea?
           , assetMint.quantity :: bigint?
        FROM chain.tx             AS tx
        JOIN nextSlot                          USING (slotNo)
        JOIN txIds                             USING (id)
        JOIN chain.block          AS block     ON block.id = tx.blockId AND block.slotNo = tx.slotNo
        LEFT JOIN chain.assetMint AS assetMint ON assetMint.txId = tx.id AND assetMint.slotNo = tx.slotNo
       WHERE block.rollbackToBlock IS NULL
    |]
        foldTxs
  case initialResult of
    (Nothing, _) -> pure MoveWait
    (Just header, txs) -> do
      let txIds = Map.keysSet txs
      txIns <- queryTxInsBulk txIds
      txOuts <- queryTxOutsBulk txIds
      let insOuts = Map.intersectionWith (,) txIns txOuts
      pure $
        MoveArrive header $
          Set.fromList $
            fmap snd $
              Map.toList $
                Map.intersectionWith (\(ins, outs) tx -> tx{inputs = ins, outputs = outs}) insOuts txs
  where
    params =
      ( V.fromList $ fst <$> addressParts
      , V.fromList $ snd <$> addressParts
      , pointSlot point
      )
    addressParts =
      Set.toList (NESet.toSet credentials) >>= \case
        PaymentKeyCredential pkh ->
          (,unPaymentKeyHash pkh) . BS.pack . pure
            <$> if networkId == C.Mainnet then [0x01, 0x21, 0x41, 0x61] else [0x00, 0x20, 0x40, 0x60]
        ScriptCredential sh ->
          (,unScriptHash sh) . BS.pack . pure
            <$> if networkId == C.Mainnet then [0x11, 0x31, 0x51, 0x71] else [0x10, 0x30, 0x50, 0x70]
    foldTxs :: Fold ReadTxRow (Maybe BlockHeader, Map TxId Transaction)
    foldTxs = Fold foldTxs' (Nothing, mempty) id

    foldTxs'
      :: (Maybe BlockHeader, Map TxId Transaction)
      -> ReadTxRow
      -> (Maybe BlockHeader, Map TxId Transaction)
    foldTxs' (blockHeader, txs) row@(Just slotNo, Just hash, Just blockNo, Just txId, _, _, _, _, _, _) =
      ( blockHeader <|> Just (BlockHeader (decodeSlotNo slotNo) (BlockHeaderHash hash) (decodeBlockNo blockNo))
      , Map.alter (mergeRow row) (TxId txId) txs
      )
    foldTxs' x _ = x

    mergeRow :: ReadTxRow -> Maybe Transaction -> Maybe Transaction
    mergeRow row@(_, _, _, Just txId, validityLowerBound, validityUpperBound, mMetadata, policyId, tokenName, quantity) =
      Just . \case
        Nothing ->
          Transaction
            { txId = TxId txId
            , validityRange = case (validityLowerBound, validityUpperBound) of
                (Nothing, Nothing) -> Unbounded
                (Just lb, Nothing) -> MinBound $ decodeSlotNo lb
                (Nothing, Just ub) -> MaxBound $ decodeSlotNo ub
                (Just lb, Just ub) -> MinMaxBound (decodeSlotNo lb) $ decodeSlotNo ub
            , metadata = maybe mempty fromCardanoTxMetadata $ decodeMetadata =<< mMetadata
            , inputs = Set.empty
            , outputs = []
            , mintedTokens = decodeTokens policyId tokenName quantity
            }
        Just tx -> mergeTxRow tx row
    mergeRow _ = const Nothing

mergeTxRow :: Transaction -> ReadTxRow -> Transaction
mergeTxRow tx@Transaction{mintedTokens} (_, _, _, _, _, _, _, policyId, tokenName, quantity) =
  tx
    { mintedTokens = mintedTokens <> decodeTokens policyId tokenName quantity
    }

queryTxIns :: SlotNo -> TxId -> HT.Transaction (Set.Set TransactionInput)
queryTxIns slotNo txInId =
  HT.statement (unTxId txInId, fromIntegral slotNo) $
    [foldStatement|
    SELECT txIn.txOutId :: bytea
         , txIn.txOutIx :: smallint
         , txOut.address :: bytea
         , txOut.datumBytes :: bytea?
         , txIn.redeemerDatumBytes :: bytea?
      FROM chain.txIn  as txIn
      JOIN chain.txOut as txOut ON txOut.txId = txIn.txOutId AND txOut.txIx = txIn.txOutIx
     WHERE txIn.txInId = $1 :: bytea AND txIn.slotNo = $2 :: bigint
  |]
      (Fold.foldMap (Set.singleton . decodeTxIn) id)

decodeTxIn :: ReadTxInRow -> TransactionInput
decodeTxIn (txId, txIx, address, datumBytes, redeemerDatumBytes) =
  TransactionInput
    { txId = TxId txId
    , txIx = fromIntegral txIx
    , address = Address address
    , datumBytes = fromPlutusData . C.toPlutusData . unsafeDeserialize' <$> datumBytes
    , redeemer = Redeemer . fromPlutusData . C.toPlutusData . unsafeDeserialize' <$> redeemerDatumBytes
    }

queryTxOuts :: SlotNo -> TxId -> HT.Transaction [TransactionOutput]
queryTxOuts slotNo txId =
  HT.statement (unTxId txId, fromIntegral slotNo) $
    [foldStatement|
    SELECT txOut.txIx :: smallint
         , txOut.address :: bytea
         , txOut.lovelace :: bigint
         , txOut.datumHash :: bytea?
         , txOut.datumBytes :: bytea?
         , assetOut.policyId :: bytea?
         , assetOut.name :: bytea?
         , assetOut.quantity :: bigint?
      FROM chain.txOut         AS txOut
      LEFT JOIN chain.assetOut AS assetOut ON assetOut.txOutId = txOut.txId AND assetOut.txOutIx = txOut.txIx
     WHERE txOut.txId = $1 :: bytea AND txOut.slotNo = $2 :: bigint
     ORDER BY txIx
  |]
      (Fold foldRow IntMap.empty (fmap snd . IntMap.toAscList))
  where
    foldRow :: IntMap TransactionOutput -> ReadTxOutRow -> IntMap TransactionOutput
    foldRow acc (txIx, address, lovelace, datumHash, datumBytes, policyId, tokenName, quantity) =
      IntMap.alter (Just . maybe newTxOut mergeTxOut) (fromIntegral txIx) acc
      where
        newTxOut =
          TransactionOutput
            { address = Address address
            , assets =
                Assets
                  { ada = Lovelace $ fromIntegral lovelace
                  , tokens = decodeTokens policyId tokenName quantity
                  }
            , datumHash = DatumHash <$> datumHash
            , datum = fromPlutusData . C.toPlutusData . unsafeDeserialize' <$> datumBytes
            }

        mergeTxOut txOut@TransactionOutput{assets = assets@Assets{..}} =
          txOut
            { assets =
                assets
                  { tokens = tokens <> decodeTokens policyId tokenName quantity
                  }
            }

queryTxInsBulk :: Set TxId -> HT.Transaction (Map TxId (Set.Set TransactionInput))
queryTxInsBulk txInIds =
  HT.statement (V.fromList $ unTxId <$> Set.toList txInIds) $
    [foldStatement|
      SELECT txIn.txInId :: bytea
          , txIn.txOutId :: bytea
          , txIn.txOutIx :: smallint
          , txOut.address :: bytea
          , txOut.datumBytes :: bytea?
          , txIn.redeemerDatumBytes :: bytea?
        FROM chain.txIn  as txIn
        JOIN chain.txOut as txOut ON txOut.txId = txIn.txOutId AND txOut.txIx = txIn.txOutIx
      WHERE txIn.txInId = ANY($1 :: bytea[])
  |]
      foldTxIns
  where
    foldTxIns :: Fold ReadTxInBulkRow (Map TxId (Set.Set TransactionInput))
    foldTxIns = Fold foldTxIns' mempty id

    foldTxIns' :: Map TxId (Set TransactionInput) -> ReadTxInBulkRow -> Map TxId (Set TransactionInput)
    foldTxIns' txIns row@(txId, _, _, _, _, _) =
      Map.alter (Just . ($ decodeTxIn' row) . maybe Set.singleton (flip Set.insert)) (TxId txId) txIns

    decodeTxIn' :: ReadTxInBulkRow -> TransactionInput
    decodeTxIn' (_, txId, txIx, address, datumBytes, redeemerDatumBytes) =
      decodeTxIn (txId, txIx, address, datumBytes, redeemerDatumBytes)

queryTxOutsBulk :: Set TxId -> HT.Transaction (Map TxId [TransactionOutput])
queryTxOutsBulk txIds =
  HT.statement (V.fromList $ unTxId <$> Set.toList txIds) $
    [foldStatement|
      SELECT txOut.txId :: bytea
          , txOut.txIx :: smallint
          , txOut.address :: bytea
          , txOut.lovelace :: bigint
          , txOut.datumHash :: bytea?
          , txOut.datumBytes :: bytea?
          , assetOut.policyId :: bytea?
          , assetOut.name :: bytea?
          , assetOut.quantity :: bigint?
        FROM chain.txOut         AS txOut
        LEFT JOIN chain.assetOut AS assetOut ON assetOut.txOutId = txOut.txId AND assetOut.txOutIx = txOut.txIx
      WHERE txOut.txId = ANY($1 :: bytea[])
      ORDER BY txIx
  |]
      (Fold foldRow mempty (fmap $ fmap snd . IntMap.toAscList))
  where
    foldRow
      :: Map TxId (IntMap TransactionOutput)
      -> ReadTxOutBulkRow
      -> Map TxId (IntMap TransactionOutput)
    foldRow acc (txId, txIx, address, lovelace, datumHash, datumBytes, policyId, tokenName, quantity) =
      Map.alter
        ( Just
            . maybe
              (IntMap.singleton (fromIntegral txIx) newTxOut)
              (IntMap.alter (Just . maybe newTxOut mergeTxOut) (fromIntegral txIx))
        )
        (TxId txId)
        acc
      where
        newTxOut =
          TransactionOutput
            { address = Address address
            , assets =
                Assets
                  { ada = Lovelace $ fromIntegral lovelace
                  , tokens = decodeTokens policyId tokenName quantity
                  }
            , datumHash = DatumHash <$> datumHash
            , datum = fromPlutusData . C.toPlutusData . unsafeDeserialize' <$> datumBytes
            }

        mergeTxOut txOut@TransactionOutput{assets = assets@Assets{..}} =
          txOut
            { assets =
                assets
                  { tokens = tokens <> decodeTokens policyId tokenName quantity
                  }
            }

-- GetTip

getTip :: GetTip HT.Transaction
getTip =
  DB.GetTip $
    decodeChainPoint
      <$> HT.statement
        ()
        [singletonStatement|
    SELECT slotNo :: bigint, id :: bytea, blockNo :: bigint
      FROM chain.block
      WHERE rollbackToSlot IS NULL
      ORDER BY slotNo DESC
      LIMIT 1
  |]

-- GetUTxOs

getUTxOs :: GetUTxOs HT.Transaction
getUTxOs =
  Database.GetUTxOs $
    fmap UTxOs . \case
      GetUTxOsForTxOutRefs txOutRefs -> do
        let -- Passing an array of anonymous composite types by zipping it through `unnest`:
            -- https://github.com/nikita-volkov/hasql/issues/25#issuecomment-286053459
            txOutRefTuple (TxOutRef (TxId txId) (TxIx txIx)) = (txId, fromIntegral txIx)
            txOutRefs' = (V.fromList *** V.fromList) . unzip . fmap txOutRefTuple . Set.toList $ txOutRefs
        HT.statement txOutRefs' $
          [foldStatement|
            WITH txOuts (txId, txIx) AS
              ( SELECT * FROM UNNEST($1 :: bytea[], $2 :: smallint[])
              )
            SELECT txOut.txId :: bytea
                , txOut.txIx :: smallint
                , txOut.address :: bytea
                , txOut.lovelace :: bigint
                , txOut.datumHash :: bytea?
                , txOut.datumBytes :: bytea?
                , assetOut.policyId :: bytea?
                , assetOut.name :: bytea?
                , assetOut.quantity :: bigint?
              FROM chain.txOut         AS txOut
              NATURAL JOIN txOuts
              LEFT JOIN chain.txIn     AS txIn     ON txIn.txOutId = txOut.txId AND txIn.txOutIx = txOut.txIx
              LEFT JOIN chain.assetOut AS assetOut ON assetOut.txOutId = txOut.txId AND assetOut.txOutIx = txOut.txIx
            WHERE txIn.txInId IS NULL
            ORDER BY txIx
          |]
            (Fold foldRow mempty id)
      GetUTxOsAtAddresses addresses -> do
        let addresses' = V.fromList $ fmap unAddress . Set.toList $ addresses
        HT.statement addresses' $
          [foldStatement|
            WITH addresses (address) AS
              ( SELECT * FROM UNNEST($1 :: bytea[])
              )
            SELECT txOut.txId :: bytea
                , txOut.txIx :: smallint
                , txOut.address :: bytea
                , txOut.lovelace :: bigint
                , txOut.datumHash :: bytea?
                , txOut.datumBytes :: bytea?
                , assetOut.policyId :: bytea?
                , assetOut.name :: bytea?
                , assetOut.quantity :: bigint?
              FROM chain.txOut         AS txOut
              JOIN addresses           AS addr     ON addr.address = txOut.address AND CAST(MD5(addr.address) AS uuid) = CAST(MD5(txOut.address) AS uuid)
              LEFT JOIN chain.txIn     AS txIn     ON txIn.txOutId = txOut.txId AND txIn.txOutIx = txOut.txIx
              LEFT JOIN chain.assetOut AS assetOut ON assetOut.txOutId = txOut.txId AND assetOut.txOutIx = txOut.txIx
            WHERE txIn.txInId IS NULL
            ORDER BY txIx
          |]
            (Fold foldRow mempty id)
  where
    foldRow acc (txId, txIx, address, lovelace, datumHash, datumBytes, policyId, tokenName, quantity) =
      Map.alter (Just . maybe newTxOut mergeTxOut) (TxOutRef (TxId txId) (fromIntegral txIx)) acc
      where
        newTxOut =
          TransactionOutput
            { address = Address address
            , assets =
                Assets
                  { ada = Lovelace $ fromIntegral lovelace
                  , tokens = decodeTokens policyId tokenName quantity
                  }
            , datumHash = DatumHash <$> datumHash
            , datum = fromPlutusData . C.toPlutusData . unsafeDeserialize' <$> datumBytes
            }

        mergeTxOut txOut@TransactionOutput{assets = assets@Assets{..}} =
          txOut
            { assets =
                assets
                  { tokens = tokens <> decodeTokens policyId tokenName quantity
                  }
            }

decodeTokens :: Maybe ByteString -> Maybe ByteString -> Maybe Int64 -> Tokens
decodeTokens (Just policyId) (Just tokenName) (Just quantity) =
  Tokens $
    Map.singleton
      (AssetId (PolicyId policyId) (TokenName tokenName))
      (Quantity $ fromIntegral quantity)
decodeTokens _ _ _ = mempty

type ReadTxRow =
  ( Maybe Int64 -- Tx Block's SlotNo
  , Maybe ByteString -- Tx Block's BlockHeaderHash
  , Maybe Int64 -- Tx Block's BlockNo
  , Maybe ByteString -- Tx id
  , Maybe Int64 -- Tx validityLowerBound
  , Maybe Int64 -- Tx validityUpperBound
  , Maybe ByteString -- Tx metadata
  , Maybe ByteString -- Tx minted Token's PolicyId
  , Maybe ByteString -- Tx minted Token's TokenName
  , Maybe Int64 -- Tx minted Token's Quantity
  )

type ReadTxInRow =
  ( ByteString -- TxIn's TxId
  , Int16 -- TxIn's TxIx
  , ByteString -- TxIn's Address
  , Maybe ByteString -- TxIn's datumBytes
  , Maybe ByteString -- TxIn's redeemerDatumBytes
  )

type ReadTxInBulkRow =
  ( ByteString -- TxIn's Tx' TxId
  , ByteString -- TxIn's TxId
  , Int16 -- TxIn's TxIx
  , ByteString -- TxIn's Address
  , Maybe ByteString -- TxIn's datumBytes
  , Maybe ByteString -- TxIn's redeemerDatumBytes
  )

type ReadTxOutRow =
  ( Int16 -- TxOut's TxIx
  , ByteString -- TxOut's Address
  , Int64 -- TxOut's Lovelace
  , Maybe ByteString -- TxOut's DatumHash
  , Maybe ByteString -- TxOut's datumBytes
  , Maybe ByteString -- TxOut's Token's PolicyId
  , Maybe ByteString -- TxOut's Token's TokenName
  , Maybe Int64 -- TxOut's Token's Quantity
  )

type ReadTxOutBulkRow =
  ( ByteString -- TxOut's TxId
  , Int16 -- TxOut's TxIx
  , ByteString -- TxOut's Address
  , Int64 -- TxOut's Lovelace
  , Maybe ByteString -- TxOut's DatumHash
  , Maybe ByteString -- TxOut's datumBytes
  , Maybe ByteString -- TxOut's Token's PolicyId
  , Maybe ByteString -- TxOut's Token's TokenName
  , Maybe Int64 -- TxOut's Token's Quantity
  )

performIntersect :: [BlockHeader] -> ChainPoint -> HT.Transaction (PerformMoveResult IntersectError ())
performIntersect points point = do
  let params =
        ( V.fromList $ (\BlockHeader{..} -> fromIntegral slotNo) <$> points
        , V.fromList $ (\BlockHeader{..} -> unBlockHeaderHash headerHash) <$> points
        , pointSlot point
        )
  decodeAdvance
    <$> HT.statement
      params
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

decodeBlockHeader :: (Int64, ByteString, Int64) -> BlockHeader
decodeBlockHeader (slotNo, hash, blockNo) =
  BlockHeader (decodeSlotNo slotNo) (BlockHeaderHash hash) (decodeBlockNo blockNo)

decodeChainPoint :: (Int64, ByteString, Int64) -> ChainPoint
decodeChainPoint (-1, _, _) = Genesis
decodeChainPoint row = At $ decodeBlockHeader row

decodeSlotNo :: Int64 -> SlotNo
decodeSlotNo = SlotNo . fromIntegral

decodeBlockNo :: Int64 -> BlockNo
decodeBlockNo = BlockNo . fromIntegral

pointSlot :: ChainPoint -> Int64
pointSlot Genesis = -1
pointSlot (At BlockHeader{..}) = fromIntegral slotNo

decodeAdvance :: Maybe (Int64, ByteString, Int64) -> PerformMoveResult err ()
decodeAdvance Nothing = MoveWait
decodeAdvance (Just row) = MoveArrive (decodeBlockHeader row) ()

decodeMetadata :: ByteString -> Maybe C.TxMetadata
decodeMetadata = hush . C.deserialiseFromCBOR C.AsTxMetadata
