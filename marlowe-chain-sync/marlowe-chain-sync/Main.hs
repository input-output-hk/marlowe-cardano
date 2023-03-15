{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}


module Main
  where

import Cardano.Api
  ( CardanoEra(..)
  , CardanoMode
  , ConsensusModeParams(..)
  , EpochSlots(..)
  , EraInMode(..)
  , LocalNodeConnectInfo(..)
  , TxInMode(..)
  )
import qualified Cardano.Api as Cardano (connectToLocalNode)
import Control.Concurrent.Component
import Control.Exception (bracket)
import Control.Monad ((<=<))
import Data.String (IsString(fromString))
import qualified Data.Text.Lazy.IO as TL
import Data.UUID.V4 (nextRandom)
import Hasql.Pool (UsageError(..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient(..), NodeClientDependencies(..), nodeClient)
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Connection (SomeConnectionSource(..), logConnectionSource)
import Network.Protocol.Driver (TcpServerDependencies(..), tcpServer)
import Network.Protocol.Handshake.Server (handshakeConnectionSource)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Server (queryServerPeer)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Observe.Event.Explicit (injectSelector, narrowEventBackend)
import Options (Options(..), getOptions)
import System.IO (stderr)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release $
  runComponent_ proc pool -> do
    eventBackend <- logger -< LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = injectSelector ConfigWatcher
      }

    syncSource <- tcpServer -< TcpServerDependencies
      { host
      , port
      , toPeer = chainSeekServerPeer
      }

    querySource <- tcpServer -< TcpServerDependencies
      { host
      , port = queryPort
      , toPeer = queryServerPeer
      }

    jobSource <- tcpServer -< TcpServerDependencies
      { host
      , port = commandPort
      , toPeer = jobServerPeer
      }

    NodeClient{..} <- nodeClient -< NodeClientDependencies
      { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
      , eventBackend = narrowEventBackend (injectSelector NodeService) eventBackend
      }

    chainSync -< ChainSyncDependencies
      { databaseQueries = hoistDatabaseQueries
          (either throwUsageError pure <=< Pool.use pool)
          $ PostgreSQL.databaseQueries networkId
      , syncSource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend (injectSelector ChainSeekServer) eventBackend)
          $ handshakeConnectionSource syncSource
      , querySource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend (injectSelector QueryServer) eventBackend)
          $ handshakeConnectionSource querySource
      , jobSource = SomeConnectionSource
          $ logConnectionSource (narrowEventBackend (injectSelector JobServer) eventBackend)
          $ handshakeConnectionSource jobSource
      , queryLocalNodeState = queryNode
      , submitTxToNodeLocal = \era tx -> submitTxToNode $ TxInMode tx case era of
          ByronEra -> ByronEraInCardanoMode
          ShelleyEra -> ShelleyEraInCardanoMode
          AllegraEra -> AllegraEraInCardanoMode
          MaryEra -> MaryEraInCardanoMode
          AlonzoEra -> AlonzoEraInCardanoMode
          BabbageEra -> BabbageEraInCardanoMode
      , httpPort = fromIntegral httpPort
      }
  where
    throwUsageError (ConnectionUsageError err)                       = error $ show err
    throwUsageError (SessionUsageError (Session.QueryError _ _ err)) = error $ show err
    throwUsageError AcquisitionTimeoutUsageError                     = error "hasql-timeout"

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- The epoch slots ignored after Byron.
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }
