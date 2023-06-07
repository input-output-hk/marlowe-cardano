{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime
  where

import Cardano.Api (CardanoEra(..), CardanoMode, EraInMode(..), LocalNodeClientProtocolsInMode, TxInMode(..))
import Control.Arrow (returnA)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (Probes(..))
import Control.Monad (foldM, (<=<))
import Control.Monad.Event.Class
import Data.Time (NominalDiffTime)
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database as ChainIndexer
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel, NodeClientSelector)
import Language.Marlowe.Runtime.ChainIndexer.Store (ChainStoreSelector)
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncCommand, ChainSyncQuery, RuntimeChainSeekClient, RuntimeChainSeekServer)
import qualified Language.Marlowe.Runtime.ChainSync.Database as ChainSync
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient(..), NodeClientDependencies(..), nodeClient)
import qualified Language.Marlowe.Runtime.ChainSync.NodeClient as Sync
import Network.Protocol.ChainSeek.Client (serveChainSeekClient)
import Network.Protocol.Connection (ClientServerPair(connectionSource), clientServerPair)
import Network.Protocol.Job.Client (JobClient, serveJobClient)
import Network.Protocol.Job.Server (JobServer)
import Network.Protocol.Query.Client (QueryClient, serveQueryClient)
import Network.Protocol.Query.Server (QueryServer)
import UnliftIO
  ( Concurrently(Concurrently)
  , MonadIO
  , MonadUnliftIO
  , STM
  , SomeException(SomeException)
  , atomically
  , liftIO
  , newTVar
  , readTVar
  , try
  , withRunInIO
  , writeTVar
  )

data MarloweRuntimeDependencies r m = MarloweRuntimeDependencies
  { connectToLocalNode :: !((r -> LocalNodeClientProtocolsInMode CardanoMode) -> m ())
  , maxCost :: !Int
  , costModel :: !CostModel
  , chainIndexerDatabaseQueries :: !(ChainIndexer.DatabaseQueries m)
  , chainSyncDatabaseQueries :: !(ChainSync.DatabaseQueries m)
  , persistRateLimit :: !NominalDiffTime
  , genesisBlock :: !GenesisBlock
  }

marloweRuntime
  :: ( MonadUnliftIO m
     , MonadFail m
     , MonadEvent r s m
     , Inject (ChainStoreSelector r) s
     , Inject NodeClientSelector s
     , Inject Sync.NodeClientSelector s
     )
  => Component m (MarloweRuntimeDependencies r m) Probes
marloweRuntime = proc MarloweRuntimeDependencies{..} -> do
  Channels{..} <- setupChannels -< ()

  NodeClient{..} <- unnestNodeClient <$> supervisor "node-client" nodeClient -< NodeClientDependencies
    { connectToLocalNode = \client -> withRunInIO \runInIO -> runInIO $ connectToLocalNode $ const client
    }

  chainIndexerProbes <- supervisor "chain-indexer" chainIndexer -< ChainIndexerDependencies
    { databaseQueries = chainIndexerDatabaseQueries
    , ..
    }

  chainSyncProbes <- supervisor "chain-sync" chainSync -< ChainSyncDependencies
    { databaseQueries = chainSyncDatabaseQueries
    , syncSource = connectionSource chainSeekChannel
    , querySource = connectionSource chainQueryChannel
    , jobSource = connectionSource chainJobChannel
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

  returnA -< mergeProbes [chainIndexerProbes, chainSyncProbes]

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
  pure (pure (), Channels{..})

-- | Restarts a component when it crashes. The output is an action to retrieve the output value of the currently running
-- instance of the component.
supervisor :: MonadUnliftIO m => String -> Component m a b -> Component m a (STM b)
supervisor name (Component f) = Component \a -> do
  (Concurrently initialAction, initialOutput) <- f a
  currentOutput <- newTVar initialOutput
  let
    supervisedAction action = do
      liftIO $ putStrLn $ "Supervisor starting component " <> name
      result <- try action
      case result of
        Left (SomeException _) -> do
          liftIO $ putStrLn $ "Supervised component " <> name <> " crashed, restarting"
          Concurrently action' <- atomically do
            (action', output) <- f a
            writeTVar currentOutput output
            pure action'
          supervisedAction action'
        Right () -> do
          liftIO $ putStrLn $ "Supervised component " <> name <> " finished"
          pure ()
  pure (Concurrently $ supervisedAction initialAction, readTVar currentOutput)

mergeProbes :: [STM Probes] -> Probes
mergeProbes probes = Probes
  { liveness = foldM (\acc -> fmap (acc &&) . liveness <=< atomically) True probes
  , readiness = foldM (\acc -> fmap (acc &&) . readiness <=< atomically) True probes
  , startup = pure True
  }

data Channels m = Channels
  { chainSeekChannel :: ClientServerPair RuntimeChainSeekServer RuntimeChainSeekClient m
  , chainQueryChannel :: ClientServerPair (QueryServer ChainSyncQuery) (QueryClient ChainSyncQuery) m
  , chainJobChannel :: ClientServerPair (JobServer ChainSyncCommand) (JobClient ChainSyncCommand) m
  }
