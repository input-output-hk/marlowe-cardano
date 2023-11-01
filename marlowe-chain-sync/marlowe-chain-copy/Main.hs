{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Cardano.Api (
  Block (..),
  BlockHeader (..),
  BlockInMode (..),
  BlockNo (..),
  CardanoMode,
  ChainPoint (..),
  ChainSyncClient (ChainSyncClient),
  ChainTip (..),
  ConsensusModeParams (CardanoModeParams),
  EpochSlots (EpochSlots),
  File (..),
  LocalChainSyncClient (LocalChainSyncClient),
  LocalNodeClientProtocols (
    LocalNodeClientProtocols,
    localChainSyncClient,
    localStateQueryClient,
    localTxMonitoringClient,
    localTxSubmissionClient
  ),
  LocalNodeConnectInfo (LocalNodeConnectInfo, localConsensusModeParams, localNodeNetworkId, localNodeSocketPath),
  connectToLocalNode,
 )
import Cardano.Api.ChainSync.Client (ClientStIdle (..), ClientStNext (..))
import Control.Monad (join, when)
import Data.ByteString.Lazy (toChunks)
import Data.Csv (ToRecord)
import Data.Csv.Incremental (encode, encodeRecord)
import Data.Foldable (for_, traverse_)
import Data.Functor (void)
import Data.Int (Int64)
import Data.String (IsString (..))
import Data.Version (showVersion)
import Database.PostgreSQL.Simple (Connection, Query, close, connectPostgreSQL, execute_)
import Database.PostgreSQL.Simple.Copy (copy_, putCopyData, putCopyEnd, putCopyError)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Cardano (blockToRows)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types
import Numeric.Natural (Natural)
import Options
import Paths_marlowe_chain_sync (version)
import UnliftIO (
  Concurrently (Concurrently, runConcurrently),
  Exception (displayException),
  MonadIO (..),
  MonadUnliftIO,
  SomeException (..),
  TBQueue,
  atomically,
  bracket,
  finally,
  mask,
  newTBQueueIO,
  onException,
  readTBQueue,
  throwIO,
  try,
  writeTBQueue,
 )

maxBlocksInQueue :: Natural
maxBlocksInQueue = 1024

maxTxsInQueue :: Natural
maxTxsInQueue = maxBlocksInQueue * 8

maxTxInsInQueue :: Natural
maxTxInsInQueue = maxTxsInQueue * 8

maxTxOutsInQueue :: Natural
maxTxOutsInQueue = maxTxsInQueue * 8

maxAssetOutsInQueue :: Natural
maxAssetOutsInQueue = maxTxOutsInQueue * 4

maxAssetMintsInQueue :: Natural
maxAssetMintsInQueue = maxTxsInQueue * 4

main :: IO ()
main = do
  Options{..} <- getOptions $ showVersion version
  blockQueue <- newTBQueueIO maxBlocksInQueue
  blockRowQueue <- newTBQueueIO maxBlocksInQueue
  txRowQueue <- newTBQueueIO maxTxsInQueue
  txOutRowQueue <- newTBQueueIO maxTxOutsInQueue
  txInRowQueue <- newTBQueueIO maxTxInsInQueue
  assetOutRowQueue <- newTBQueueIO maxAssetOutsInQueue
  assetMintRowQueue <- newTBQueueIO maxAssetMintsInQueue
  bracket (truncateTablesAndDisableIndexes databaseUri) enableIndexes \_ -> runConcurrently do
    Concurrently $
      runBlockProcessor
        blockQueue
        blockRowQueue
        txRowQueue
        txOutRowQueue
        txInRowQueue
        assetOutRowQueue
        assetMintRowQueue
    Concurrently $ runCopy databaseUri "block (id, slotNo, blockNo)" blockRowQueue
    Concurrently $
      runCopy databaseUri "tx (blockId, id, slotNo, validityLowerBound, validityUpperBound, metadata, isValid)" txRowQueue
    Concurrently $
      runCopy
        databaseUri
        "txOut (txId, txIx, slotNo, address, lovelace, datumHash, datumBytes, isCollateral, addressHeader, addressPaymentCredential, addressStakeAddressReference)"
        txOutRowQueue
    Concurrently $
      runCopy databaseUri "txIn (txOutId, txOutIx, txInId, slotNo, redeemerDatumBytes, isCollateral)" txInRowQueue
    Concurrently $ runCopy databaseUri "assetOut (txOutId, txOutIx, slotNo, policyId, name, quantity)" assetOutRowQueue
    Concurrently $ runCopy databaseUri "assetMint (txId, slotNo, policyId, name, quantity)" assetMintRowQueue
    Concurrently $
      runChainSync
        blockQueue
        LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
          , localNodeNetworkId = networkId
          , localNodeSocketPath = File nodeSocket
          }
    pure ()

runBlockProcessor
  :: TBQueueMaybe (BlockInMode CardanoMode)
  -> TBQueueMaybe BlockRow
  -> TBQueueMaybe TxRow
  -> TBQueueMaybe TxOutRow
  -> TBQueueMaybe TxInRow
  -> TBQueueMaybe AssetOutRow
  -> TBQueueMaybe AssetMintRow
  -> IO ()
runBlockProcessor blockQueue blockRowQueue txRowQueue txOutRowQueue txInRowQueue assetOutRowQueue assetMintRowQueue = go
  where
    go = join $ atomically do
      mBlock <- readTBQueue blockQueue
      case mBlock of
        Nothing -> do
          writeTBQueue blockRowQueue Nothing
          writeTBQueue txRowQueue Nothing
          writeTBQueue txOutRowQueue Nothing
          writeTBQueue txInRowQueue Nothing
          writeTBQueue assetOutRowQueue Nothing
          writeTBQueue assetMintRowQueue Nothing
          pure $ pure ()
        Just block -> do
          let (blockRow, txRows) = blockToRows block
          writeTBQueue blockRowQueue $ Just blockRow
          for_ txRows \(txRow, txInRows, txOutRows, txMintRows) -> do
            writeTBQueue txRowQueue $ Just txRow
            traverse_ (writeTBQueue txInRowQueue . Just) txInRows
            for_ txOutRows \(txOutRow, assetOutRows) -> do
              writeTBQueue txOutRowQueue $ Just txOutRow
              traverse_ (writeTBQueue assetOutRowQueue . Just) assetOutRows
            traverse_ (writeTBQueue assetMintRowQueue . Just) txMintRows
          pure go

type TBQueueMaybe a = TBQueue (Maybe a)

runChainSync :: TBQueueMaybe (BlockInMode CardanoMode) -> LocalNodeConnectInfo CardanoMode -> IO ()
runChainSync blockQueue connect = do
  connectToLocalNode
    connect
    LocalNodeClientProtocols
      { localChainSyncClient = LocalChainSyncClient $ ChainSyncClient $ pure idle
      , localTxSubmissionClient = Nothing
      , localStateQueryClient = Nothing
      , localTxMonitoringClient = Nothing
      }
  where
    idle :: ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    idle = SendMsgRequestNext next end

    next :: ClientStNext (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    next =
      ClientStNext
        { recvMsgRollForward = \block tip -> ChainSyncClient do
            case block of
              BlockInMode (Block (BlockHeader _ _ (BlockNo blockNo)) _) _ -> case tip of
                ChainTip _ _ (BlockNo blockNo') -> do
                  let onePercentOfTip = blockNo' `div` 100
                  when (blockNo `mod` onePercentOfTip == 0) do
                    let percent = blockNo `div` onePercentOfTip
                    putStrLn $
                      "Copying block #"
                        <> show blockNo
                        <> " of "
                        <> show blockNo'
                        <> " ("
                        <> show percent
                        <> "%)"
                _ -> pure ()
            atomically $ writeTBQueue blockQueue $ Just block
            pure idle
        , recvMsgRollBackward = \point _ -> ChainSyncClient $ pure case point of
            ChainPointAtGenesis -> idle
            _ -> SendMsgDone ()
        }

    end :: IO (ClientStNext (BlockInMode CardanoMode) ChainPoint ChainTip IO ())
    end = do
      atomically $ writeTBQueue blockQueue Nothing
      pure
        ClientStNext
          { recvMsgRollForward = \_ _ -> ChainSyncClient $ pure $ SendMsgDone ()
          , recvMsgRollBackward = \_ _ -> ChainSyncClient $ pure $ SendMsgDone ()
          }

truncateTablesAndDisableIndexes :: String -> IO Connection
truncateTablesAndDisableIndexes dbUri = do
  conn <- connectPostgreSQL $ fromString dbUri
  flip onException (close conn) do
    void $ execute_ conn "TRUNCATE TABLE chain.block, chain.tx, chain.txOut, chain.txIn, chain.assetOut, chain.assetMint"
    void $
      execute_
        conn
        [sql|
          WITH indexes (indexId) AS
            ( SELECT (schemaName||'.'||indexName)
              FROM pg_indexes
              WHERE schemaName = 'chain'
            )
          UPDATE pg_index
          SET indisready = FALSE
          FROM indexes
          WHERE indexrelid = indexId::regclass
      |]
  pure conn

enableIndexes :: Connection -> IO ()
enableIndexes conn = void $ flip finally (close conn) do
  _ <-
    execute_
      conn
      [sql|
      WITH indexes (indexId) AS
        ( SELECT (schemaName||'.'||indexName)
          FROM pg_indexes
          WHERE schemaName = 'chain'
        )
      UPDATE pg_index
      SET indisready = TRUE
      FROM indexes
      WHERE NOT indisready
        AND indexrelid = indexId::regclass
    |]
  putStrLn "Indexing chain schema"
  execute_ conn "REINDEX SCHEMA chain"

runCopy :: (ToRecord a) => String -> Query -> TBQueueMaybe a -> IO Int64
runCopy dbUri table rowQueue = withConnection dbUri \conn -> mask \restore -> do
  copy_ conn $ "COPY chain." <> table <> " FROM STDIN WITH (FORMAT 'csv')"
  result <- try $ restore $ do
    let go = do
          mRow <- atomically $ readTBQueue rowQueue
          case mRow of
            Nothing -> pure ()
            Just row -> do
              traverse_ (putCopyData conn) $ toChunks $ encode $ encodeRecord row
              go
    go
  case result of
    Left (SomeException ex) -> do
      putCopyError conn $ fromString $ displayException ex
      throwIO ex
    Right _ -> do
      putCopyEnd conn

withConnection :: (MonadUnliftIO m) => String -> (Connection -> m a) -> m a
withConnection uri = bracket (liftIO $ connectPostgreSQL $ fromString uri) (liftIO . close)
