{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}

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
  AddressInEra (..),
  AddressTypeInEra (..),
  AsType (..),
  AssetId (..),
  AssetName (..),
  Block (..),
  BlockHeader (..),
  BlockInMode (..),
  BlockNo (..),
  CardanoMode,
  ChainPoint (..),
  CtxTx,
  EraInMode,
  IsCardanoEra,
  Lovelace (..),
  PolicyId (..),
  Quantity (Quantity),
  ScriptValidity (..),
  SerialiseAsCBOR (serialiseToCBOR),
  SerialiseAsRawBytes (..),
  ShelleyBasedEra (ShelleyBasedEraAlonzo, ShelleyBasedEraBabbage),
  SlotNo (..),
  Tx,
  TxBody (..),
  TxBodyContent (..),
  TxId,
  TxIn (..),
  TxInsCollateral (..),
  TxIx (..),
  TxMetadataInEra (..),
  TxMintValue (..),
  TxOut (..),
  TxOutValue (..),
  TxReturnCollateral (..),
  TxScriptValidity (..),
  TxValidityLowerBound (..),
  TxValidityUpperBound (..),
  getTxBody,
  getTxId,
  selectLovelace,
  valueToList,
 )
import Cardano.Api.Shelley (Address (..), Hash (..), Tx (..), toShelleyTxIn)
import Cardano.Binary (serialize, serialize')
import qualified Cardano.Ledger.Alonzo.Scripts.Data as Alonzo
import qualified Cardano.Ledger.Alonzo.Tx as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody as Alonzo
import qualified Cardano.Ledger.Alonzo.TxWits as Alonzo
import qualified Cardano.Ledger.Babbage.Tx as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import Cardano.Ledger.SafeHash (originalBytes)
import Cardano.Ledger.Shelley.API.Types (Credential (..), Network (..), StakeReference (..), StrictMaybe (..))
import Control.Monad.Event.Class (MonadInjectEvent, withEvent)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (ask)
import Data.Bits ((.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Base16 (encodeBase16)
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Short (fromShort, toShort)
import Data.Csv (ToField (..), ToRecord)
import Data.Csv.Incremental (Builder, encode, encodeRecord)
import Data.Foldable (toList, traverse_)
import Data.Int (Int16, Int64)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import Data.Profunctor (rmap)
import qualified Data.Set as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (diffUTCTime, getCurrentTime)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.Copy (copy, putCopyData, putCopyEnd, putCopyError)
import qualified Database.PostgreSQL.Simple.Internal as PS
import Database.PostgreSQL.Simple.Transaction (withTransactionSerializable)
import qualified Database.PostgreSQL.Simple.Types as PS
import GHC.Generics (Generic)
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
  CardanoBlock,
  CommitBlocks (..),
  CommitGenesisBlock (..),
  CommitRollback (..),
  DatabaseQueries (DatabaseQueries),
  GetGenesisBlock (..),
  GetIntersectionPoints (..),
  hoistCommitBlocks,
  hoistCommitGenesisBlock,
  hoistCommitRollback,
  hoistGetGenesisBlock,
  hoistGetIntersectionPoints,
 )
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock (..), GenesisTx (..))
import Observe.Event (addField)
import Ouroboros.Network.Point (WithOrigin (..))
import UnliftIO (Exception (displayException), SomeException (..), mask, newIORef, newMVar, throwIO, try)
import Prelude hiding (init)

data QuerySelector f where
  Query :: Text -> QuerySelector QueryField

data QueryField
  = SqlStatement ByteString
  | Parameters [Text]
  | Operation Text

-- | PostgreSQL implementation for the chain sync database queries.
databaseQueries
  :: forall r s m. (MonadInjectEvent r QuerySelector s m, MonadIO m) => Pool -> GenesisBlock -> DatabaseQueries m
databaseQueries pool genesisBlock =
  DatabaseQueries
    (hoistCommitRollback (transact "commitRollback" "INSERT" TS.Write) $ commitRollback genesisBlock)
    (hoistCommitBlocks (runSession "commitBlocks" "INSERT") commitBlocks)
    (hoistCommitGenesisBlock (transact "commitGenesisBlock" "INSERT" TS.Write) commitGenesisBlock)
    (hoistGetIntersectionPoints (transact "getIntersectionPoints" "SELECT" TS.Read) getIntersectionPoints)
    (hoistGetGenesisBlock (transact "getGenesisBlock" "SELECT" TS.Read) getGenesisBlock)
  where
    transact :: Text -> Text -> TS.Mode -> Transaction a -> m a
    transact operation queryName = fmap (runSession operation queryName) . TS.transaction TS.Serializable

    runSession :: Text -> Text -> Session.Session a -> m a
    runSession operation queryName m = withEvent (Query queryName) \ev -> do
      addField ev $ Operation operation
      result <- liftIO $ Pool.use pool m
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

commitRollback :: GenesisBlock -> CommitRollback Transaction
commitRollback GenesisBlock{..} = CommitRollback \point -> do
  let slotNo = case point of
        ChainPointAtGenesis -> -1
        ChainPoint s _ -> slotNoToParam s
      hash = case point of
        ChainPointAtGenesis -> genesisBlockHash
        ChainPoint _ h -> h
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
commitBlocks :: CommitBlocks Session.Session
commitBlocks = CommitBlocks \blocks -> do
  let txs = extractTxs =<< blocks
      txOuts = extractTxOuts =<< txs
      txIns = extractTxIns =<< txs
      assetOuts = extractAssetOuts =<< txOuts
      assetMints = extractAssetMints =<< txs
  sessionConnection <- ask
  pre <- liftIO getCurrentTime
  assetIds <- TS.transaction TS.Serializable TS.Write $ copyAssets assetOuts assetMints
  post <- liftIO getCurrentTime
  liftIO $ putStrLn $ "INSERT chain.asset took " <> show (diffUTCTime post pre)
  liftIO $ withLibPQConnection sessionConnection \libPqConnection -> do
    connectionHandle <- newMVar libPqConnection
    connectionObjects <- newMVar mempty
    connectionTempNameCounter <- newIORef 0
    let connection = PS.Connection{..}
    withTransactionSerializable connection do
      copyBlocks connection blocks
      copyTxs connection txs
      copyTxOuts connection txOuts
      copyTxIns connection txIns
      copyAssetOuts assetIds connection assetOuts
      copyAssetMints assetIds connection assetMints
  where
    extractTxs :: CardanoBlock -> [SomeTx]
    extractTxs (BlockInMode (Block (BlockHeader slotNo hash _) blockTxs) era) = flip (SomeTx hash slotNo) era <$> blockTxs

    extractTxOuts :: SomeTx -> [SomeTxOut]
    extractTxOuts (SomeTx _ slotNo tx era) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txScriptValidity of
        TxScriptValidity _ ScriptInvalid -> case txReturnCollateral of
          TxReturnCollateralNone -> []
          TxReturnCollateral _ txOut ->
            let -- The index of the `TxOut` for collateral change on a transaction that
                -- fails due to an invalid script is one more than the number of `TxOut`s
                -- that there would have been had the transaction succeeded.
                index = toEnum $ length txOuts
             in [SomeTxOut (getTxId body) (TxIx index) slotNo txOut (Nothing, Nothing) True era]
        _ -> do
          (ix, txOut, datumInfo) <- zip3 [0 ..] txOuts datums
          pure $ SomeTxOut (getTxId body) (TxIx ix) slotNo txOut datumInfo False era
      where
        datums = case tx of
          ShelleyTx ShelleyBasedEraAlonzo (Alonzo.AlonzoTx body wits _ _) ->
            let getDatum (Alonzo.AlonzoTxOut _ _ (SJust dh)) =
                  ( Just $ originalBytes dh
                  , fmap serialize' . M.lookup dh . Alonzo.unTxDats $ Alonzo.txdats' wits
                  )
                getDatum (Alonzo.AlonzoTxOut _ _ SNothing) = (Nothing, Nothing)
             in toList $ getDatum <$> Alonzo.outputs' body
          ShelleyTx ShelleyBasedEraBabbage (Alonzo.AlonzoTx body wits _ _) ->
            let getDatum (Babbage.BabbageTxOut _ _ datum _) =
                  case datum of
                    Babbage.NoDatum -> (Nothing, Nothing)
                    Babbage.DatumHash dh ->
                      ( Just $ originalBytes dh
                      , fmap serialize' . M.lookup dh . Alonzo.unTxDats $ Babbage.txdats' wits
                      )
                    Babbage.Datum d ->
                      ( Just . originalBytes $ Alonzo.hashBinaryData d
                      , Just . serialize' $ Alonzo.binaryDataToData d
                      )
             in toList $ getDatum <$> Babbage.outputs' body
          _ -> (Nothing, Nothing) <$ (let TxBody TxBodyContent{txOuts} = getTxBody tx in txOuts)

    extractTxIns :: SomeTx -> [SomeTxIn]
    extractTxIns (SomeTx _ slotNo tx _) = case getTxBody tx of
      body@(TxBody TxBodyContent{..}) -> case txScriptValidity of
        TxScriptValidity _ ScriptInvalid -> case txInsCollateral of
          TxInsCollateralNone -> []
          TxInsCollateral _ collateralIns -> do
            TxIn txId txIx <- collateralIns
            pure $ SomeTxIn txId txIx (getTxId body) slotNo Nothing True
        _ -> do
          txIn@(TxIn txId txIx) <- fst <$> txIns
          pure $ SomeTxIn txId txIx (getTxId body) slotNo (getRedeemer txIn) False
      where
        getRedeemer :: TxIn -> Maybe ByteString
        getRedeemer = case tx of
          ShelleyTx ShelleyBasedEraAlonzo alonzoTx@Alonzo.AlonzoTx{} ->
            \txIn -> do
              (datum, _) <- Alonzo.indexedRdmrs alonzoTx $ Alonzo.Spending $ toShelleyTxIn txIn
              pure $ originalBytes $ Alonzo.dataToBinaryData datum
          ShelleyTx ShelleyBasedEraBabbage babbageTx -> do
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
            AdaAssetId -> []
            AssetId policyId name -> [AssetMint (getTxId body) slotNo policyId name quantity]

    extractAssetOuts :: SomeTxOut -> [AssetOut]
    extractAssetOuts (SomeTxOut txId ix slotNo (TxOut _ value _ _) _ _ _) = case value of
      TxOutAdaOnly _ _ -> []
      TxOutValue _ value' -> do
        (assetId, quantity) <- valueToList value'
        case assetId of
          AdaAssetId -> []
          AssetId policyId name -> [AssetOut txId ix slotNo policyId name quantity]

newtype Bytea = Bytea ByteString

instance ToField Bytea where
  toField (Bytea bs) = "\\x" <> encodeUtf8 (encodeBase16 bs)

data BlockRow = BlockRow
  { hash :: !Bytea
  , slotNo :: !Int64
  , blockNo :: !Int64
  }
  deriving (Generic)

instance ToRecord BlockRow

copyBlocks :: PS.Connection -> [CardanoBlock] -> IO ()
copyBlocks conn =
  copyBuilder conn "block (id, slotNo, blockNo)"
    . foldMap (encodeRecord . blockRow)

data SomeTx
  = forall era.
    (IsCardanoEra era) =>
    SomeTx (Hash BlockHeader) SlotNo (Tx era) (EraInMode era CardanoMode)

newtype SqlBool = SqlBool Bool

instance ToField SqlBool where
  toField (SqlBool True) = "TRUE"
  toField (SqlBool False) = "FALSE"

data TxRow = TxRow
  { blockHash :: !Bytea
  , txId :: !Bytea
  , slotNo :: !Int64
  , validityLowerBound :: !(Maybe Int64)
  , validityUpperBound :: !(Maybe Int64)
  , metadata :: !(Maybe Bytea)
  , isValid :: !SqlBool
  }
  deriving (Generic)

instance ToRecord TxRow

copyTxs :: PS.Connection -> [SomeTx] -> IO ()
copyTxs conn =
  copyBuilder conn "tx (blockId, id, slotNo, validityLowerBound, validityUpperBound, metadata, isValid)"
    . foldMap (encodeRecord . txRow)

data SomeTxOut
  = forall era.
    (IsCardanoEra era) =>
    SomeTxOut TxId TxIx SlotNo (TxOut CtxTx era) (Maybe ByteString, Maybe ByteString) Bool (EraInMode era CardanoMode)

data TxOutRow = TxOutRow
  { txId :: !Bytea
  , txIx :: !Int16
  , slotNo :: !Int64
  , address :: !Bytea
  , lovelace :: !Int64
  , datumHash :: !(Maybe Bytea)
  , datumBytes :: !(Maybe Bytea)
  , isCollateral :: !SqlBool
  , addressHeader :: !Bytea
  , addressPaymentCredential :: !(Maybe Bytea)
  , addressStakeAddressReference :: !(Maybe Bytea)
  }
  deriving (Generic)

instance ToRecord TxOutRow

copyTxOuts :: PS.Connection -> [SomeTxOut] -> IO ()
copyTxOuts conn =
  copyBuilder
    conn
    "txOut (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral, addressHeader, addressPaymentCredential, addressStakeAddressReference)"
    . foldMap (encodeRecord . txOutRow)

data SomeTxIn = SomeTxIn TxId TxIx TxId SlotNo (Maybe ByteString) Bool

data TxInRow = TxInRow
  { txOutId :: !Bytea
  , txOutIx :: !Int16
  , txInId :: !Bytea
  , slotNo :: !Int64
  , redeemerDatumBytes :: !(Maybe Bytea)
  , isCollateral :: !SqlBool
  }
  deriving (Generic)

instance ToRecord TxInRow

copyTxIns :: PS.Connection -> [SomeTxIn] -> IO ()
copyTxIns conn =
  copyBuilder conn "txIn (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral)"
    . foldMap (encodeRecord . txInRow)

type Asset = (ByteString, ByteString)

copyAssets :: [AssetOut] -> [AssetMint] -> Transaction (Map (ByteString, ByteString) Int)
copyAssets assetOuts assetMints = do
  let assets =
        Set.toList $
          foldMap (Set.singleton . outAsset) assetOuts <> foldMap (Set.singleton . mintAsset) assetMints
  foldMap (\(policyId, name, assetId) -> Map.singleton (policyId, name) $ fromIntegral assetId)
    <$> HT.statement
      (V.fromList $ fst <$> assets, V.fromList $ snd <$> assets)
      [vectorStatement|
        WITH inputRows (policyId, name) AS
          ( SELECT * FROM UNNEST ($1 :: bytea[], $2 :: bytea[])
          )
        , inserts (policyId, name, id) AS
          ( INSERT into chain.asset (policyId, name)
            SELECT *
            FROM inputRows
            ON CONFLICT (policyId, name) DO NOTHING
            RETURNING policyId, name, id
          )
        SELECT policyId :: bytea, name :: bytea, id :: int
        FROM inserts
        UNION ALL
        SELECT asset.policyId :: bytea, asset.name :: bytea, asset.id :: int
        FROM inputRows
        JOIN chain.asset USING (policyId, name)
    |]

outAsset :: AssetOut -> Asset
outAsset (AssetOut _ _ _ policy (AssetName name) _) = (serialiseToRawBytes policy, name)

mintAsset :: AssetMint -> Asset
mintAsset (AssetMint _ _ policy (AssetName name) _) = (serialiseToRawBytes policy, name)

data AssetOut = AssetOut TxId TxIx SlotNo PolicyId AssetName Quantity

data AssetOutRow = AssetOutRow
  { txId :: !Bytea
  , txIx :: !Int16
  , slotNo :: !Int64
  , assetId :: !Int
  , quantity :: !Int64
  }
  deriving (Generic)

instance ToRecord AssetOutRow

copyAssetOuts :: Map (ByteString, ByteString) Int -> PS.Connection -> [AssetOut] -> IO ()
copyAssetOuts assetIds conn =
  copyBuilder conn "assetOut (txOutId, txOutIx, slotNo, assetId, quantity)"
    . foldMap (encodeRecord . assetOutRow assetIds)

data AssetMint = AssetMint TxId SlotNo PolicyId AssetName Quantity

data AssetMintRow = AssetMintRow
  { txId :: !Bytea
  , slotNo :: !Int64
  , assetId :: !Int
  , quantity :: !Int64
  }
  deriving (Generic)

instance ToRecord AssetMintRow

copyAssetMints :: Map (ByteString, ByteString) Int -> PS.Connection -> [AssetMint] -> IO ()
copyAssetMints assetIds conn =
  copyBuilder conn "assetMint (txId, slotNo, assetId, quantity)"
    . foldMap (encodeRecord . assetMintRow assetIds)

copyBuilder :: (ToRecord a) => PS.Connection -> ByteString -> Builder a -> IO ()
copyBuilder conn table builder = mask \restore -> do
  let query = "COPY chain." <> PS.Query table <> " FROM STDIN WITH (FORMAT 'csv')"
  pre <- getCurrentTime
  copy conn query ()
  result <- try $ restore $ traverse_ (putCopyData conn) $ BL.toChunks $ encode builder
  case result of
    Left (SomeException ex) -> do
      putCopyError conn $ fromString $ displayException ex
      throwIO ex
    Right _ -> do
      count <- putCopyEnd conn
      post <- getCurrentTime
      let elapsed = diffUTCTime post pre
      putStr $ "[" <> show elapsed <> "] [" <> show count <> "Rows] "
      putStrLn $ "COPY " <> show table
      pure ()

blockRow :: CardanoBlock -> BlockRow
blockRow (BlockInMode (Block (BlockHeader slotNo hash (BlockNo blockNo)) _) _) =
  BlockRow
    { hash = Bytea $ headerHashToParam hash
    , slotNo = slotNoToParam slotNo
    , blockNo = fromIntegral blockNo
    }

txRow :: SomeTx -> TxRow
txRow (SomeTx blockHash slotNo tx _) = case getTxBody tx of
  body@(TxBody TxBodyContent{txValidityRange, txMetadata, txScriptValidity}) ->
    TxRow
      { blockHash = Bytea $ headerHashToParam blockHash
      , txId = Bytea $ serialiseToRawBytes $ getTxId body
      , slotNo = slotNoToParam slotNo
      , validityLowerBound = case fst txValidityRange of
          TxValidityNoLowerBound -> Nothing
          TxValidityLowerBound _ slot -> Just $ slotNoToParam slot
      , validityUpperBound = case snd txValidityRange of
          TxValidityNoUpperBound _ -> Nothing
          TxValidityUpperBound _ slot -> Just $ slotNoToParam slot
      , metadata =
          Bytea <$> case txMetadata of
            TxMetadataNone -> Nothing
            TxMetadataInEra _ metadata -> Just $ serialiseToCBOR metadata
      , isValid = SqlBool case txScriptValidity of
          TxScriptValidity _ ScriptInvalid -> False
          _ -> True
      }

txOutRow :: SomeTxOut -> TxOutRow
txOutRow (SomeTxOut txId txIx slotNo (TxOut address value _ _) (datumBytesHashed, datumBytes) isCollateral _) =
  TxOutRow
    { txId = Bytea $ serialiseToRawBytes txId
    , txIx = txIxToParam txIx
    , slotNo = slotNoToParam slotNo
    , address = Bytea $ serialiseToRawBytes address
    , lovelace = lovelaceToParam case value of
        TxOutAdaOnly _ lovelace -> lovelace
        TxOutValue _ value' -> selectLovelace value'
    , datumHash = Bytea <$> datumBytesHashed
    , datumBytes = Bytea <$> datumBytes
    , isCollateral = SqlBool isCollateral
    , addressHeader = case address of
        AddressInEra (ShelleyAddressInEra _) (ShelleyAddress network payment stake) ->
          -- See https://cips.cardano.org/cips/cip19/#binaryFormat
          let networkTag = case network of
                Testnet -> 0b00000000
                Mainnet -> 0b00000001
              headerType = case (payment, stake) of
                (KeyHashObj{}, StakeRefBase KeyHashObj{}) -> 0b00000000
                (ScriptHashObj{}, StakeRefBase KeyHashObj{}) -> 0b00010000
                (KeyHashObj{}, StakeRefBase ScriptHashObj{}) -> 0b00100000
                (ScriptHashObj{}, StakeRefBase ScriptHashObj{}) -> 0b00110000
                (KeyHashObj{}, StakeRefPtr{}) -> 0b01000000
                (ScriptHashObj{}, StakeRefPtr{}) -> 0b01010000
                (KeyHashObj{}, StakeRefNull) -> 0b01100000
                (ScriptHashObj{}, StakeRefNull) -> 0b01110000
           in Bytea $ BS.pack [headerType .|. networkTag]
        _ -> Bytea $ BS.pack $ pure $ BS.head $ serialiseToRawBytes address
    , addressPaymentCredential = case address of
        AddressInEra (ShelleyAddressInEra _) (ShelleyAddress _ payment _) -> Just $ Bytea $ BL.toStrict case payment of
          KeyHashObj keyHash -> serialize keyHash
          ScriptHashObj scriptHash -> serialize scriptHash
        _ -> Nothing
    , addressStakeAddressReference = case address of
        AddressInEra (ShelleyAddressInEra _) (ShelleyAddress _ _ stake) -> case stake of
          StakeRefNull -> Nothing
          StakeRefBase (KeyHashObj keyHash) -> Just $ Bytea $ BL.toStrict $ serialize keyHash
          StakeRefBase (ScriptHashObj scriptHash) -> Just $ Bytea $ BL.toStrict $ serialize scriptHash
          StakeRefPtr ptr -> Just $ Bytea $ BL.toStrict $ serialize ptr
        _ -> Nothing
    }

txInRow :: SomeTxIn -> TxInRow
txInRow (SomeTxIn txOutId txOutIx txInId txInSlotNo redeemerDatumBytes isCollateral) =
  TxInRow
    { txOutId = Bytea $ serialiseToRawBytes txOutId
    , txOutIx = txIxToParam txOutIx
    , txInId = Bytea $ serialiseToRawBytes txInId
    , slotNo = slotNoToParam txInSlotNo
    , redeemerDatumBytes = Bytea <$> redeemerDatumBytes
    , isCollateral = SqlBool isCollateral
    }

assetOutRow :: Map (ByteString, ByteString) Int -> AssetOut -> AssetOutRow
assetOutRow assetIds (AssetOut txId txIx slotNo policyId (AssetName name) (Quantity quantity)) =
  AssetOutRow
    { txId = Bytea $ serialiseToRawBytes txId
    , txIx = txIxToParam txIx
    , slotNo = slotNoToParam slotNo
    , assetId = fromJust $ Map.lookup (serialiseToRawBytes policyId, name) assetIds
    , quantity = fromIntegral quantity
    }

assetMintRow :: Map (ByteString, ByteString) Int -> AssetMint -> AssetMintRow
assetMintRow assetIds (AssetMint txId slotNo policyId (AssetName name) (Quantity quantity)) =
  AssetMintRow
    { txId = Bytea $ serialiseToRawBytes txId
    , slotNo = slotNoToParam slotNo
    , assetId = fromJust $ Map.lookup (serialiseToRawBytes policyId, name) assetIds
    , quantity = fromIntegral quantity
    }

headerHashToParam :: Hash BlockHeader -> ByteString
headerHashToParam (HeaderHash hash) = fromShort hash

lovelaceToParam :: Lovelace -> Int64
lovelaceToParam (Lovelace slotNo) = fromIntegral slotNo

slotNoToParam :: SlotNo -> Int64
slotNoToParam (SlotNo slotNo) = fromIntegral $ min slotNo (fromIntegral (maxBound :: Int64))

txIxToParam :: TxIx -> Int16
txIxToParam (TxIx txIx) = fromIntegral txIx
