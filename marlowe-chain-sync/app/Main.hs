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
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Arrow (arr)
import Control.Category ((<<<))
import Control.Concurrent.Component
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Data.String (IsString(fromString))
import Data.Text (unpack)
import Data.Time (secondsToNominalDiffTime)
import Hasql.Pool (UsageError(..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSyncDependencies(..), chainSync)
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis(..), codecChainSeek)
import Language.Marlowe.Runtime.ChainSync.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.Genesis (computeByronGenesisBlock)
import Logging (RootSelector(ChainSeekServer), logger)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Driver (acceptRunServerPeerOverSocket, acceptRunServerPeerOverSocketWithLogging)
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
import Options (Options(..), getOptions)

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
        genesisConfigResult <- runExceptT do
          hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
          (hash,) <$> withExceptT
            (const "failed to read byron genesis file")
            (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
        (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
        let genesisBlock = computeByronGenesisBlock (abstractHashToBytes hash) genesisConfig
        let
          chainSyncDependencies eventBackend = ChainSyncDependencies
            { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
            , databaseQueries = hoistDatabaseQueries
                (either throwUsageError pure <=< Pool.use pool)
                (PostgreSQL.databaseQueries genesisBlock)
            , persistRateLimit
            , genesisBlock
            , acceptRunChainSeekServer = acceptRunServerPeerOverSocketWithLogging
                (narrowEventBackend ChainSeekServer eventBackend)
                throwIO
                chainSeekSocket
                codecChainSeek
                (chainSeekServerPeer Genesis)
            , acceptRunQueryServer = acceptRunServerPeerOverSocket throwIO querySocket codecQuery queryServerPeer
            , acceptRunJobServer = acceptRunServerPeerOverSocket throwIO commandSocket codecJob jobServerPeer
            , queryLocalNodeState = queryNodeLocalState localNodeConnectInfo
            , submitTxToNodeLocal = \era tx -> Cardano.submitTxToNodeLocal localNodeConnectInfo $ TxInMode tx case era of
                ByronEra -> ByronEraInCardanoMode
                ShelleyEra -> ShelleyEraInCardanoMode
                AllegraEra -> AllegraEraInCardanoMode
                MaryEra -> MaryEraInCardanoMode
                AlonzoEra -> AlonzoEraInCardanoMode
                BabbageEra -> BabbageEraInCardanoMode
            , maxCost
            , costModel
            }
          loggerDependencies = ()
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

    persistRateLimit = secondsToNominalDiffTime 1

    resolve p = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show p)

    open addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket
