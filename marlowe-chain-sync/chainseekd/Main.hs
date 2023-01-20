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
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad ((<=<))
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
import Network.Protocol.Driver (acceptRunServerPeerOverSocketWithLogging)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket
  ( AddrInfo(..)
  , AddrInfoFlag(..)
  , SocketOption(ReuseAddr)
  , SocketType(..)
  , bind
  , close
  , defaultHints
  , getAddrInfo
  , listen
  , openSocket
  , setCloseOnExecIfNeeded
  , setSocketOption
  , withFdSocket
  , withSocketsDo
  )
import Observe.Event (narrowEventBackend)
import Observe.Event.Backend (newOnceFlagMVar)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Options (Options(..), getOptions)
import System.IO (stderr)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  chainSeekAddr <- resolve port
  queryAddr <- resolve queryPort
  commandAddr <- resolve commandPort
  bracket (open chainSeekAddr) close \chainSeekSocket -> do
    bracket (open queryAddr) close \querySocket -> do
      bracket (open commandAddr) close \commandSocket -> do
        pool <- Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)
        let
          chainSyncDependencies eventBackend = ChainSyncDependencies
            { databaseQueries = hoistDatabaseQueries
                (either throwUsageError pure <=< Pool.use pool)
                PostgreSQL.databaseQueries
            , acceptRunChainSeekServer = acceptRunServerPeerOverSocketWithLogging
                (narrowEventBackend ChainSeekServer eventBackend)
                throwIO
                chainSeekSocket
                codecChainSeek
                (chainSeekServerPeer Genesis)
            , acceptRunQueryServer = acceptRunServerPeerOverSocketWithLogging
                (narrowEventBackend QueryServer eventBackend)
                throwIO
                querySocket
                codecQuery
                queryServerPeer
            , acceptRunJobServer = acceptRunServerPeerOverSocketWithLogging
                (narrowEventBackend JobServer eventBackend)
                throwIO
                commandSocket
                codecJob
                jobServerPeer
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
        runComponent_ appComponent loggerDependencies
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

    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

    open addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket
