{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime
  where

import Cardano.Api (CardanoEra(..), CardanoMode, EraInMode(..), LocalNodeClientProtocolsInMode, NetworkId, TxInMode(..))
import Colog (Message, WithLog, logInfo, logWarning)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (Probes(..))
import Control.Monad.Event.Class
import Data.Set (Set)
import Data.Set.NonEmpty (NESet)
import Data.String (fromString)
import Data.Time (NominalDiffTime)
import Language.Marlowe.Protocol.HeaderSync.Server (MarloweHeaderSyncServer, marloweHeaderSyncServerPeer)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import qualified Language.Marlowe.Protocol.HeaderSync.Types as HeaderSync
import Language.Marlowe.Protocol.Load.Server (MarloweLoadServer, marloweLoadServerPeer)
import Language.Marlowe.Protocol.Load.Types (MarloweLoad)
import qualified Language.Marlowe.Protocol.Load.Types as Load
import Language.Marlowe.Protocol.Query.Types (MarloweSyncRequest)
import Language.Marlowe.Protocol.Server (MarloweRuntimeServer)
import Language.Marlowe.Protocol.Sync.Server (MarloweSyncServer, marloweSyncServerPeer)
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import qualified Language.Marlowe.Protocol.Sync.Types as MarloweSync
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel, NodeClientSelector)
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector)
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockNo, ChainSyncCommand, ChainSyncQuery(GetUTxOs), RuntimeChainSeekClient, RuntimeChainSeekServer, ScriptHash)
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient(..), NodeClientDependencies(..), nodeClient)
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as Sync
import Language.Marlowe.Runtime.Contract (ContractDependencies(..), contract)
import Language.Marlowe.Runtime.Contract.Api (ContractRequest)
import Language.Marlowe.Runtime.Contract.Store (ContractStore)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts)
import Language.Marlowe.Runtime.Indexer (MarloweIndexerDependencies(..), marloweIndexer)
import qualified Language.Marlowe.Runtime.Indexer.ChainSeekClient as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.Database as MarloweIndexer
import qualified Language.Marlowe.Runtime.Indexer.Store as MarloweIndexer
import Language.Marlowe.Runtime.Proxy (ProxyDependenciesInProc(..), RouterInProc(..), proxyInProc)
import Language.Marlowe.Runtime.Sync (SyncDependencies(..), sync)
import qualified Language.Marlowe.Runtime.Sync.Database as MarloweSync
import Language.Marlowe.Runtime.Transaction (transaction)
import qualified Language.Marlowe.Runtime.Transaction as Tx
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Language.Marlowe.Runtime.Transaction.Query
  (LoadMarloweContextSelector, LoadWalletContextSelector, loadMarloweContext, loadWalletContext)
import Language.Marlowe.Runtime.Transaction.Server (TransactionServerSelector)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJobDependencies(..), mkSubmitJob)
import Network.Channel.Typed (ChannelServerPair(..), ClientServerPair(..), channelServerPair, clientServerPair)
import Network.Protocol.ChainSeek.Client (serveChainSeekClient)
import Network.Protocol.Connection (ConnectionSource, runConnector)
import Network.Protocol.Job.Client (JobClient, serveJobClient)
import Network.Protocol.Job.Server (JobServer, jobServerPeer)
import Network.Protocol.Job.Types (Job)
import qualified Network.Protocol.Job.Types as Job
import Network.Protocol.Query.Client (QueryClient, request, serveQueryClient)
import Network.Protocol.Query.Server (QueryServer, queryServerPeer)
import Network.Protocol.Query.Types (Query)
import qualified Network.Protocol.Query.Types as Query
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
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  , getScripts :: forall v. MarloweVersion v -> Set MarloweScripts
  , submitConfirmationBlocks :: BlockNo
  , networkId :: NetworkId
  , runtimeConnectionSourceTraced :: ConnectionSource MarloweRuntimeServer m
  , runtimeConnectionSource :: ConnectionSource MarloweRuntimeServer m
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
  => Component m (MarloweRuntimeDependencies r n m) Probes
marloweRuntime = proc MarloweRuntimeDependencies{..} -> do
  Channels{..} <- setupChannels -< ()

  NodeClient{..} <- unnestNodeClient <$> supervisor "node-client" nodeClient -< NodeClientDependencies
    { connectToLocalNode = \client -> withRunInIO \runInIO -> runInIO $ connectToLocalNode $ const client
    }

  supervisor "chain-indexer" chainIndexer -< ChainIndexerDependencies
    { databaseQueries = chainIndexerDatabaseQueries
    , ..
    }

  supervisor "chain-sync" chainSync -< ChainSyncDependencies
    { databaseQueries = chainSyncDatabaseQueries
    , syncSource = clientServerSource chainSeekChannel
    , querySource = clientServerSource chainQueryChannel
    , jobSource = clientServerSource chainJobChannel
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

  supervisor "marlowe-indexer" marloweIndexer -< MarloweIndexerDependencies
    { databaseQueries = marloweIndexerDatabaseQueries
    , chainSyncConnector = clientServerConnector chainSeekChannel
    , chainSyncQueryConnector = clientServerConnector chainQueryChannel
    , ..
    }

  supervisor "marlowe-sync" sync -< SyncDependencies
    { databaseQueries = marloweSyncDatabaseQueries
    , syncSource = channelServerSource marloweSyncChannel
    , headerSyncSource = channelServerSource marloweHeaderSyncChannel
    , querySource = channelServerSource marloweQueryChannel
    }

  supervisor "marlowe-contract" contract -< ContractDependencies
    { loadSource = channelServerSource marloweLoadChannel
    , querySource = channelServerSource contractQueryChannelServerChannel <> clientServerSource contractQueryClientServerChannel
    , ..
    }

  supervisor "marlowe-tx" transaction -< Tx.TransactionDependencies
    { chainSyncConnector = clientServerConnector chainSeekChannel
    , connectionSource = channelServerSource txJobChannel
    , mkSubmitJob = mkSubmitJob SubmitJobDependencies
        { chainSyncConnector = clientServerConnector chainSeekChannel
        , chainSyncJobConnector = clientServerConnector chainJobChannel
        , pollingInterval = 1.5
        , confirmationTimeout = 3600 -- 1 hour
        , ..
        }
    , loadWalletContext = loadWalletContext
        $ runConnector (clientServerConnector chainQueryChannel) . request . GetUTxOs
    , loadMarloweContext = loadMarloweContext getScripts networkId (clientServerConnector chainSeekChannel) $ clientServerConnector chainQueryChannel
    , chainSyncQueryConnector = clientServerConnector chainQueryChannel
    , contractQueryConnector = clientServerConnector contractQueryClientServerChannel
    , ..
    }

  unnestProbes <$> supervisor "marlowe-proxy" proxyInProc -< ProxyDependenciesInProc
    { router = RouterInProc
        { connectMarloweSync = channelServerChannel marloweSyncChannel
        , connectMarloweHeaderSync = channelServerChannel marloweHeaderSyncChannel
        , connectMarloweQuery = channelServerChannel marloweQueryChannel
        , connectMarloweLoad = channelServerChannel marloweLoadChannel
        , connectTxJob = channelServerChannel txJobChannel
        , connectContractQuery = channelServerChannel contractQueryChannelServerChannel
        }
    , connectionSource = runtimeConnectionSource
    , connectionSourceTraced = runtimeConnectionSourceTraced
    }

unnestNodeClient :: MonadIO m => STM (NodeClient m) -> NodeClient m
unnestNodeClient mNodeClient = NodeClient
  { queryNode = \point query -> do
      NodeClient{..} <- atomically mNodeClient
      queryNode point query
  , submitTxToNode = \tx -> do
      NodeClient{..} <- atomically mNodeClient
      submitTxToNode tx
  }

setupChannels :: MonadUnliftIO m => Component m () (Channels m)
setupChannels = Component $ const do
  chainSeekChannel <- clientServerPair serveChainSeekClient
  chainQueryChannel <- clientServerPair serveQueryClient
  chainJobChannel <- clientServerPair serveJobClient
  marloweSyncChannel <- channelServerPair marloweSyncServerPeer
  marloweHeaderSyncChannel <- channelServerPair marloweHeaderSyncServerPeer
  marloweQueryChannel <- channelServerPair queryServerPeer
  marloweLoadChannel <- channelServerPair marloweLoadServerPeer
  contractQueryClientServerChannel <- clientServerPair serveQueryClient
  contractQueryChannelServerChannel <- channelServerPair queryServerPeer
  txJobChannel <- channelServerPair jobServerPeer
  pure (pure (), Channels{..})

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

data Channels m = Channels
  { chainSeekChannel :: ClientServerPair RuntimeChainSeekServer RuntimeChainSeekClient m
  , chainQueryChannel :: ClientServerPair (QueryServer ChainSyncQuery) (QueryClient ChainSyncQuery) m
  , chainJobChannel :: ClientServerPair (JobServer ChainSyncCommand) (JobClient ChainSyncCommand) m
  , marloweSyncChannel :: ChannelServerPair MarloweSync 'MarloweSync.StInit MarloweSyncServer m
  , marloweHeaderSyncChannel :: ChannelServerPair MarloweHeaderSync 'HeaderSync.StIdle MarloweHeaderSyncServer m
  , marloweQueryChannel :: ChannelServerPair (Query MarloweSyncRequest) 'Query.StReq (QueryServer MarloweSyncRequest) m
  , marloweLoadChannel :: ChannelServerPair MarloweLoad ('Load.StProcessing 'Load.RootNode) MarloweLoadServer m
  , contractQueryClientServerChannel :: ClientServerPair (QueryServer ContractRequest) (QueryClient ContractRequest) m
  , contractQueryChannelServerChannel :: ChannelServerPair (Query ContractRequest) 'Query.StReq (QueryServer ContractRequest) m
  , txJobChannel ::  ChannelServerPair (Job MarloweTxCommand) 'Job.StInit (JobServer MarloweTxCommand) m
  }
