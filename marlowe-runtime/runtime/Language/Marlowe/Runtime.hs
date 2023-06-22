{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime where

import Cardano.Api (CardanoEra(..), CardanoMode, EraInMode(..), LocalNodeClientProtocolsInMode, NetworkId, TxInMode(..))
import Colog (Message, WithLog, logInfo, logWarning)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (Probes(..))
import Control.Monad.Event.Class
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.String (fromString)
import Data.Time (NominalDiffTime)
import Data.Version (Version)
import Language.Marlowe.Protocol.Server (MarloweRuntimeServerDirect)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel, NodeClientSelector)
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector)
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import qualified Language.Marlowe.Runtime.ChainSync as ChainSync
import Language.Marlowe.Runtime.ChainSync.Api (BlockNo, ChainSyncQuery(GetUTxOs), ScriptHash)
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient(..), NodeClientDependencies(..), nodeClient)
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as NodeClient
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as Sync
import Language.Marlowe.Runtime.Contract (ContractDependencies(..), contract)
import qualified Language.Marlowe.Runtime.Contract as MarloweContract
import Language.Marlowe.Runtime.Contract.Store (ContractStore)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import qualified Language.Marlowe.Runtime.Indexer.ChainSeekClient as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.Database as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.Store as MarloweIndexer
import Language.Marlowe.Runtime.Proxy (MarloweProxyInProc(..), RouterInProc(..), proxyInProc)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import qualified Language.Marlowe.Runtime.Sync as MarloweSync
import qualified Language.Marlowe.Runtime.Sync.Database as MarloweSync
import Language.Marlowe.Runtime.Transaction (transaction)
import qualified Language.Marlowe.Runtime.Transaction as MarloweTx
import qualified Language.Marlowe.Runtime.Transaction as Tx
import Language.Marlowe.Runtime.Transaction.Query
  (LoadMarloweContextSelector, LoadWalletContextSelector, loadMarloweContext, loadWalletContext)
import Language.Marlowe.Runtime.Transaction.Server (TransactionServerSelector)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJobDependencies(..), mkSubmitJob)
import Network.Protocol.ChainSeek.Client (serveChainSeekClient)
import Network.Protocol.Connection (ServerSource(..), directConnector, runConnector)
import Network.Protocol.Job.Client (serveJobClient)
import Network.Protocol.Query.Client (request, serveQueryClient)
import Network.TypedProtocol (N(..), Nat)
import UnliftIO
  ( Concurrently(Concurrently)
  , MonadIO
  , MonadUnliftIO
  , STM
  , SomeException(SomeException)
  , atomically
  , newTVar
  , readTVar
  , try
  , withRunInIO
  , writeTVar
  )

data MarloweRuntimeDependencies r n m = MarloweRuntimeDependencies
  { connectToLocalNode :: (r -> LocalNodeClientProtocolsInMode CardanoMode) -> m ()
  , batchSize :: Nat ('S n)
  , chainIndexerDatabaseQueries :: ChainIndexer.DatabaseQueries m
  , chainSyncDatabaseQueries :: ChainSync.DatabaseQueries m
  , contractStore :: ContractStore m
  , costModel :: CostModel
  , genesisBlock :: GenesisBlock
  , marloweIndexerDatabaseQueries :: MarloweIndexer.DatabaseQueries m
  , marloweScriptHashes :: NESet ScriptHash
  , marloweSyncDatabaseQueries :: MarloweSync.DatabaseQueries m
  , maxCost :: Int
  , payoutScriptHashes :: NESet ScriptHash
  , persistRateLimit :: NominalDiffTime
  , pollingInterval :: NominalDiffTime
  , confirmationTimeout :: NominalDiffTime
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  , getScripts :: forall v. MarloweVersion v -> Set MarloweScripts
  , submitConfirmationBlocks :: BlockNo
  , networkId :: NetworkId
  , runtimeVersion :: Version
  }

data MarloweRuntime m = MarloweRuntime
  { serverSource :: ServerSource MarloweRuntimeServerDirect m ()
  , probes :: Probes
  }

marloweRuntime
  :: ( MonadUnliftIO m
     , MonadFail m
     , MonadEvent r s m
     , Inject (ChainStoreSelector r) s
     , Inject NodeClientSelector s
     , Inject Sync.NodeClientSelector s
     , Inject (MarloweIndexer.ChainSeekClientSelector r) s
     , Inject MarloweIndexer.StoreSelector s
     , Inject TransactionServerSelector s
     , Inject LoadWalletContextSelector s
     , Inject LoadMarloweContextSelector s
     , WithLog env Message m
     )
  => Component m (MarloweRuntimeDependencies r n m) (MarloweRuntime m)
marloweRuntime = proc MarloweRuntimeDependencies{..} -> do
  NodeClient{..} <- unnestNodeClient <$> supervisor "node-client" nodeClient -< NodeClientDependencies
    { connectToLocalNode = \client -> withRunInIO \runInIO -> runInIO $ connectToLocalNode $ const client
    }

  supervisor "chain-indexer" chainIndexer -< ChainIndexerDependencies
    { databaseQueries = chainIndexerDatabaseQueries
    , ..
    }

  mChainSync <- supervisor "chain-sync" chainSync -< ChainSyncDependencies
    { databaseQueries = chainSyncDatabaseQueries
    , queryLocalNodeState = queryNode
    , submitTxToNodeLocal = \era tx -> submitTxToNode $ TxInMode tx case era of
        ByronEra -> ByronEraInCardanoMode
        ShelleyEra -> ShelleyEraInCardanoMode
        AllegraEra -> AllegraEraInCardanoMode
        MaryEra -> MaryEraInCardanoMode
        AlonzoEra -> AlonzoEraInCardanoMode
        BabbageEra -> BabbageEraInCardanoMode
    , ..
    }

  let chainSyncConnector = directConnector serveChainSeekClient $ unnestServerSource $ ChainSync.syncServerSource <$> mChainSync
  let chainSyncQueryConnector = directConnector serveQueryClient $ unnestServerSource $ ChainSync.queryServerSource <$> mChainSync
  let chainSyncJobConnector = directConnector serveJobClient $ unnestServerSource $ ChainSync.jobServerSource <$> mChainSync

  supervisor "marlowe-indexer" marloweIndexer -< MarloweIndexerDependencies
    { databaseQueries = marloweIndexerDatabaseQueries
    , ..
    }

  mMarloweSync <- supervisor "marlowe-sync" sync -< SyncDependencies
    { databaseQueries = marloweSyncDatabaseQueries
    , ..
    }

  let marloweSyncServerSource = unnestServerSource $ MarloweSync.syncServerSource <$> mMarloweSync
  let marloweHeaderSyncServerSource = unnestServerSource $ MarloweSync.headerSyncServerSource <$> mMarloweSync
  let marloweQueryServerSource = unnestServerSource $ MarloweSync.queryServerSource <$> mMarloweSync

  mMarloweContract <- supervisor "marlowe-contract" contract -< ContractDependencies {..}

  let marloweLoadServerSource = unnestServerSource $ MarloweContract.loadServerSource <$> mMarloweContract
  let contractQueryServerSource = unnestServerSource $ MarloweContract.queryServerSource <$> mMarloweContract
  let contractQueryConnector = directConnector serveQueryClient contractQueryServerSource

  mMarloweTx <- supervisor "marlowe-tx" transaction -< Tx.TransactionDependencies
    { mkSubmitJob = mkSubmitJob SubmitJobDependencies{..}
    , loadWalletContext = loadWalletContext $ runConnector chainSyncQueryConnector . request . GetUTxOs
    , loadMarloweContext = loadMarloweContext getScripts networkId chainSyncConnector chainSyncQueryConnector
    , ..
    }

  let txJobServerSource = unnestServerSource $ MarloweTx.serverSource <$> mMarloweTx

  unnestProxy <$> supervisor "marlowe-proxy" proxyInProc -< RouterInProc{..}

unnestProxy :: MonadIO m => STM (MarloweProxyInProc m) -> MarloweRuntime m
unnestProxy mProxy = MarloweRuntime
  { serverSource = unnestServerSource $ (\MarloweProxyInProc{..} -> proxyServerSource) <$> mProxy
  , probes = unnestProbes $ (\MarloweProxyInProc{..} -> probes) <$> mProxy
  }

unnestServerSource :: MonadIO m => STM (ServerSource server m a) -> ServerSource server m a
unnestServerSource mSource = ServerSource do
  source <- atomically mSource
  getServer source

unnestNodeClient :: MonadIO m => STM (NodeClient m) -> NodeClient m
unnestNodeClient mNodeClient = NodeClient
  { queryNode = \point query -> do
      NodeClient{..} <- atomically mNodeClient
      queryNode point query
  , submitTxToNode = \tx -> do
      NodeClient{..} <- atomically mNodeClient
      submitTxToNode tx
  , nodeTip = NodeClient.nodeTip =<< mNodeClient
  }

-- | Restarts a component when it crashes. The output is an action to retrieve the output value of the currently running
-- instance of the component.
supervisor :: (MonadUnliftIO m, WithLog env Message m) => String -> Component m a b -> Component m a (STM b)
supervisor name (Component f) = Component \a -> do
  (Concurrently initialAction, initialOutput) <- f a
  currentOutput <- newTVar initialOutput
  let
    supervisedAction action = do
      logInfo $ "Supervisor starting component " <> fromString name
      result <- try action
      case result of
        Left (SomeException _) -> do
          logWarning $ "Supervised component " <> fromString name <> " crashed, restarting"
          Concurrently action' <- atomically do
            (action', output) <- f a
            writeTVar currentOutput output
            pure action'
          supervisedAction action'
        Right () -> do
          logInfo $ "Supervised component " <> fromString name <> " finished"
          pure ()
  pure (Concurrently $ supervisedAction initialAction, readTVar currentOutput)

unnestProbes :: STM Probes -> Probes
unnestProbes probes = Probes
  { liveness = liveness =<< atomically probes
  , readiness = readiness =<< atomically probes
  , startup = startup =<< atomically probes
  }
