{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}

-- {-# LANGUAGE QuasiQuotes #-}

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

module Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL (
  QueryField (..),
  QuerySelector (..),
  databaseQueries,
) where

import Cardano.Api (
  AddressAny,
  AsType (..),
  BlockHeader (..),
  BlockNo (..),
  ChainPoint (..),
  Lovelace (..),
  SerialiseAsRawBytes (..),
  SlotNo (..),
  TxId,
 )
import Cardano.Api.Shelley (Hash (..))
import Colog (Message, WithLog, logInfo)
import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Control.Monad.Reader (ask)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Short (fromShort, toShort)
import Data.Csv (ToRecord)
import Data.Csv.Incremental (Builder, encode, encodeRecord)
import Data.Foldable (traverse_)
import Data.Function (on)
import Data.Int (Int64)
import Data.List (nubBy)
import Data.Profunctor (rmap)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Database.PostgreSQL.Simple (executeMany)
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Copy (copy, putCopyData, putCopyEnd, putCopyError)
import qualified Database.PostgreSQL.Simple.Internal as PS
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import qualified Database.PostgreSQL.Simple.Types as PS
import Hasql.Connection (withLibPQConnection)
import Hasql.Pool (Pool)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (refineResult)
import Hasql.TH (resultlessStatement, vectorStatement)
import Hasql.Transaction (Transaction)
import qualified Hasql.Transaction as HT
import qualified Hasql.Transaction.Sessions as TS
import Language.Marlowe.Runtime.ChainIndexer.Database (
  CommitBlocks (..),
  CommitGenesisBlock (..),
  CommitRollback (..),
  DatabaseQueries (DatabaseQueries),
  GetGenesisBlock (..),
  GetIntersectionPoints (..),
  hoistCommitGenesisBlock,
  hoistCommitRollback,
  hoistGetGenesisBlock,
  hoistGetIntersectionPoints,
 )
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock (..), GenesisTx (..))
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Cardano (blockToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types
import Observe.Event (addField)
import Ouroboros.Network.Point (WithOrigin (..))
import UnliftIO (
  Exception (displayException),
  MonadIO (..),
  MonadUnliftIO (..),
  SomeException (..),
  mask,
  newIORef,
  newMVar,
  throwIO,
  try,
 )
import Prelude hiding (init)

data QuerySelector f where
  Query :: Text -> QuerySelector QueryField
  CopyBlocks :: QuerySelector Int64
  CopyTxs :: QuerySelector Int64
  CopyTxOuts :: QuerySelector Int64
  CopyTxIns :: QuerySelector Int64
  CopyAssetOuts :: QuerySelector Int64
  CopyAssetMints :: QuerySelector Int64
  CopyScripts :: QuerySelector Int64

data QueryField
  = SqlStatement ByteString
  | Parameters [Text]
  | Operation Text

-- | PostgreSQL implementation for the chain sync database queries.
databaseQueries
  :: forall r s env m
   . (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m, WithLog env Message m)
  => Pool
  -> DatabaseQueries m
databaseQueries pool =
  DatabaseQueries
    (hoistCommitRollback (transact "commitRollback" "INSERT" TS.Write) commitRollback)
    ( CommitBlocks \blocks -> withRunInIO \runInIO -> do
        result <- Pool.use pool $ runCommitBlocks (commitBlocks runInIO) blocks
        either throwIO pure result
    )
    (hoistCommitGenesisBlock (transact "commitGenesisBlock" "INSERT" TS.Write) commitGenesisBlock)
    (hoistGetIntersectionPoints (transact "getIntersectionPoints" "SELECT" TS.Read) getIntersectionPoints)
    (hoistGetGenesisBlock (transact "getGenesisBlock" "SELECT" TS.Read) getGenesisBlock)
  where
    transact :: Text -> Text -> TS.Mode -> Transaction a -> m a
    transact operation queryName mode m = withEvent (Query queryName) \ev -> do
      addField ev $ Operation operation
      result <- liftIO $ Pool.use pool $ TS.transaction TS.Serializable mode m
      case result of
        Left ex -> do
          case ex of
            (Pool.SessionUsageError (Session.QueryError query params _)) -> do
              addField ev $ SqlStatement query
              addField ev $ Parameters params
            _ -> pure ()
          throwIO ex
        Right a -> pure a

-- GetGenesisBlock

getGenesisBlock :: GetGenesisBlock Transaction
getGenesisBlock =
  GetGenesisBlock $
    HT.statement () $
      refineResult
        (decodeResults . V.toList)
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
    decodeGenesisTx (_, txId, lovelace, address) =
      GenesisTx
        <$> decodeTxId txId
        <*> pure (fromIntegral lovelace)
        <*> decodeAddressAny address

decodeTxId :: ByteString -> Either Text TxId
decodeTxId txId = case deserialiseFromRawBytes AsTxId txId of
  Left err -> Left $ "Invalid TxId bytes: " <> fromString (show err)
  Right txId' -> Right txId'

decodeAddressAny :: ByteString -> Either Text AddressAny
decodeAddressAny address = case deserialiseFromRawBytes AsAddressAny address of
  Left err -> Left $ "Invalid address bytes: " <> fromString (show err)
  Right address' -> Right address'

-- GetIntersectionPoints

getIntersectionPoints :: GetIntersectionPoints Transaction
getIntersectionPoints =
  GetIntersectionPoints $
    HT.statement () $
      rmap
        decodeResults
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
      | otherwise =
          At $
            BlockHeader
              (SlotNo $ fromIntegral slotNo)
              (HeaderHash $ toShort hash)
              (BlockNo $ fromIntegral blockNo)

-- CommitRollback

commitRollback :: CommitRollback Transaction
commitRollback = CommitRollback \case
  ChainPointAtGenesis ->
    HT.statement
      ()
      [resultlessStatement|
        WITH deleteBlocks AS
          ( DELETE FROM chain.block WHERE slotNo >= 0)
        , deleteTxs AS
          ( DELETE FROM chain.tx WHERE slotNo >= 0)
        , deleteTxOuts AS
          ( DELETE FROM chain.txOut WHERE slotNo >= 0)
        , deleteTxIns AS
          ( DELETE FROM chain.txIn WHERE slotNo >= 0)
        , deleteAssetOuts AS
          ( DELETE FROM chain.assetOut WHERE slotNo >= 0)
        DELETE FROM chain.assetMint WHERE slotNo >= 0
      |]
  ChainPoint s h -> do
    let slotNo = slotNoToParam s
        hash = h
    HT.statement
      (slotNo, headerHashToParam hash)
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
  let genesisBlockTxsList = Set.toList genesisBlockTxs
      params =
        ( headerHashToParam genesisBlockHash
        , V.fromList $ serialiseToRawBytes . genesisTxId <$> genesisBlockTxsList
        , V.fromList $ serialiseToRawBytes . genesisTxAddress <$> genesisBlockTxsList
        , V.fromList $ lovelaceToParam . genesisTxLovelace <$> genesisBlockTxsList
        , V.fromList $ BS.take 2 . serialiseToRawBytes . genesisTxAddress <$> genesisBlockTxsList
        )
   in HT.statement
        params
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
commitBlocks
  :: forall r s env m
   . (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m, WithLog env Message m)
  => (forall x. m x -> IO x)
  -> CommitBlocks Session.Session
commitBlocks runInIO = CommitBlocks \blocks -> do
  liftIO $ runInIO $ logInfo $ "Saving " <> T.pack (show $ length blocks) <> " blocks"
  let blockGroups = blockToRows <$> blocks
  let (blockRows, txRows, txOutRows, txInRows, assetOutRows, assetMintRows, scripts) = flattenBlockGroups blockGroups
  sessionConnection <- ask
  liftIO $ withLibPQConnection sessionConnection \libPqConnection -> do
    connectionHandle <- newMVar libPqConnection
    connectionObjects <- newMVar mempty
    connectionTempNameCounter <- newIORef 0
    let connection = PS.Connection{..}
    withTransactionSerializable connection do
      runInIO $ copyBlocks connection blockRows
      runInIO $ copyTxs connection txRows
      runInIO $ copyTxOuts connection txOutRows
      runInIO $ copyTxIns connection txInRows
      runInIO $ copyAssetOuts connection assetOutRows
      runInIO $ copyAssetMints connection assetMintRows
      runInIO $ copyScripts connection $ nubBy (on (==) scriptHash) scripts

flattenBlockGroups
  :: [BlockRowGroup] -> ([BlockRow], [TxRow], [TxOutRow], [TxInRow], [AssetOutRow], [AssetMintRow], [ScriptRow])
flattenBlockGroups = foldr foldBlockGroup ([], [], [], [], [], [], [])
  where
    foldBlockGroup
      :: BlockRowGroup
      -> ([BlockRow], [TxRow], [TxOutRow], [TxInRow], [AssetOutRow], [AssetMintRow], [ScriptRow])
      -> ([BlockRow], [TxRow], [TxOutRow], [TxInRow], [AssetOutRow], [AssetMintRow], [ScriptRow])
    foldBlockGroup (blockRow, txGroups) (blockRows, txRows, txOutRows, txInRows, assetOutRows, assetMintRows, scriptRows) =
      (blockRow : blockRows, txRows', txOutRows', txInRows', assetOutRows', assetMintRows', scriptRows')
      where
        (txRows', txOutRows', txInRows', assetOutRows', assetMintRows', scriptRows') =
          foldr foldTxGroup (txRows, txOutRows, txInRows, assetOutRows, assetMintRows, scriptRows) txGroups

    foldTxGroup
      :: TxRowGroup
      -> ([TxRow], [TxOutRow], [TxInRow], [AssetOutRow], [AssetMintRow], [ScriptRow])
      -> ([TxRow], [TxOutRow], [TxInRow], [AssetOutRow], [AssetMintRow], [ScriptRow])
    foldTxGroup (txRow, txInRows, txOutGroups, assetMintRows, scripts) (txRows, txOutRows, txInRows', assetOutRows, assetMintRows', scripts') =
      ( txRow : txRows
      , txOutRows'
      , foldr (:) txInRows' txInRows
      , assetOutRows'
      , foldr (:) assetMintRows' assetMintRows
      , foldr (:) scripts' scripts
      )
      where
        (txOutRows', assetOutRows') =
          foldr foldTxOutGroup (txOutRows, assetOutRows) txOutGroups

    foldTxOutGroup :: TxOutRowGroup -> ([TxOutRow], [AssetOutRow]) -> ([TxOutRow], [AssetOutRow])
    foldTxOutGroup (txOutRow, assetOutRows) (txOutRows, assetOutRows') =
      (txOutRow : txOutRows, foldr (:) assetOutRows' assetOutRows)

copyBlocks :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m) => PS.Connection -> [BlockRow] -> m ()
copyBlocks conn =
  copyBuilder CopyBlocks conn "block (id, slotNo, blockNo)"
    . foldMap encodeRecord

copyTxs :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m) => PS.Connection -> [TxRow] -> m ()
copyTxs conn =
  copyBuilder CopyTxs conn "tx (blockId, id, slotNo, validityLowerBound, validityUpperBound, metadata, isValid)"
    . foldMap encodeRecord

copyTxOuts :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m) => PS.Connection -> [TxOutRow] -> m ()
copyTxOuts conn =
  copyBuilder
    CopyTxOuts
    conn
    "txOut (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral, addressHeader, addressPaymentCredential, addressStakeAddressReference)"
    . foldMap encodeRecord

copyTxIns :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m) => PS.Connection -> [TxInRow] -> m ()
copyTxIns conn =
  copyBuilder CopyTxIns conn "txIn (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral)"
    . foldMap encodeRecord

copyAssetOuts
  :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m)
  => PS.Connection
  -> [AssetOutRow]
  -> m ()
copyAssetOuts conn =
  copyBuilder CopyAssetOuts conn "assetOut (txOutId, txOutIx, slotNo, policyId, name, quantity)"
    . foldMap encodeRecord

copyAssetMints
  :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m)
  => PS.Connection
  -> [AssetMintRow]
  -> m ()
copyAssetMints conn =
  copyBuilder CopyAssetMints conn "assetMint (txId, slotNo, policyId, name, quantity)"
    . foldMap encodeRecord

copyScripts
  :: (MonadInjectEvent r QuerySelector s m, MonadUnliftIO m)
  => PS.Connection
  -> [ScriptRow]
  -> m ()
copyScripts conn rows = do
  let query = [sql| INSERT INTO chain.script VALUES (?,?,?) ON CONFLICT (id) DO NOTHING |]
  withEvent CopyScripts \ev -> do
    count <- liftIO $ executeMany conn query rows
    addField ev count
    pure ()

copyBuilder
  :: ( MonadInjectEvent r QuerySelector s m
     , MonadUnliftIO m
     , ToRecord a
     )
  => QuerySelector Int64
  -> PS.Connection
  -> ByteString
  -> Builder a
  -> m ()
copyBuilder sel conn table builder = do
  let query = "COPY chain." <> PS.Query table <> " FROM STDIN WITH (FORMAT 'csv')"
  withEvent sel \ev -> mask \restore -> do
    liftIO $ copy conn query ()
    result <- try $ restore $ liftIO $ traverse_ (putCopyData conn) $ BL.toChunks $ encode builder
    case result of
      Left (SomeException ex) -> do
        liftIO $ putCopyError conn $ fromString $ displayException ex
        throwIO ex
      Right _ -> do
        count <- liftIO $ putCopyEnd conn
        addField ev count
        pure ()

headerHashToParam :: Hash BlockHeader -> ByteString
headerHashToParam (HeaderHash hash) = fromShort hash

lovelaceToParam :: Lovelace -> Int64
lovelaceToParam (Lovelace slotNo) = fromIntegral slotNo

slotNoToParam :: SlotNo -> Int64
slotNoToParam (SlotNo slotNo) = fromIntegral $ min slotNo (fromIntegral (maxBound :: Int64))
