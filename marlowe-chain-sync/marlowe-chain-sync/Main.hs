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
  , queryNodeLocalState
  )
import qualified Cardano.Api as Cardano
import Control.Arrow (arr)
import Control.Category ((<<<))
import Control.Concurrent.Component
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (allocate, runResourceT)
import Data.String (IsString(fromString))
import qualified Data.Text.Lazy.IO as TL
import Data.Time (secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import Hasql.Pool (UsageError(..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis(..))
import Language.Marlowe.Runtime.ChainSync.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Logging (RootSelector(..), getRootSelectorConfig)
import Network.Protocol.ChainSeek.Codec (codecChainSeek)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Driver (awaitConnection, openPortConnector)
import Network.Protocol.Handshake.Server (withHandshake)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Observe.Event (narrowEventBackend)
import Observe.Event.Backend (newOnceFlagMVar)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Options (Options(..), getOptions)
import System.IO (stderr)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = runResourceT do
  chainSeekConnector <- withHandshake <$> openPortConnector host port codecChainSeek (chainSeekServerPeer Genesis)
  queryConnector <- withHandshake <$> openPortConnector host queryPort codecQuery queryServerPeer
  jobConnector <- withHandshake <$> openPortConnector host commandPort codecJob jobServerPeer
  (_, pool) <- allocate (Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)) Pool.release
  let
    chainSyncDependencies eventBackend = ChainSyncDependencies
      { databaseQueries = hoistDatabaseQueries
          (either throwUsageError pure <=< Pool.use pool)
          $ PostgreSQL.databaseQueries networkId
      , acceptRunChainSeekServer = awaitConnection (narrowEventBackend ChainSeekServer eventBackend) chainSeekConnector
      , acceptRunQueryServer = awaitConnection (narrowEventBackend QueryServer eventBackend) queryConnector
      , acceptRunJobServer = awaitConnection (narrowEventBackend JobServer eventBackend) jobConnector
      , queryLocalNodeState = queryNodeLocalState localNodeConnectInfo
      , submitTxToNodeLocal = \era tx -> Cardano.submitTxToNodeLocal localNodeConnectInfo $ TxInMode tx case era of
          ByronEra -> ByronEraInCardanoMode
          ShelleyEra -> ShelleyEraInCardanoMode
          AllegraEra -> AllegraEraInCardanoMode
          MaryEra -> MaryEraInCardanoMode
          AlonzoEra -> AlonzoEraInCardanoMode
          BabbageEra -> BabbageEraInCardanoMode
      }
    loggerDependencies = LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , newOnceFlag = newOnceFlagMVar
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = ConfigWatcher
      }
    appComponent = chainSync <<< arr chainSyncDependencies <<< logger
  liftIO $ runComponent_ appComponent loggerDependencies
  where
    throwUsageError (ConnectionError err)                       = error $ show err
    throwUsageError (SessionError (Session.QueryError _ _ err)) = error $ show err

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- FIXME read from config - what is the appropriate value?
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }
