module Main where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..),
                    queryNodeLocalState)
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVar)
import Control.Exception (bracket, bracketOnError, finally, throwIO)
import Control.Monad (join, (<=<))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import qualified Data.Text.Lazy.IO as TL
import Data.Time (secondsToNominalDiffTime)
import Hasql.Pool (UsageError (..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSync (..), ChainSyncDependencies (..), mkChainSync)
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis (..), runtimeChainSeekCodec)
import Language.Marlowe.Runtime.ChainSync.Database (DatabaseQueries (..), hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.Genesis (computeByronGenesisBlock)
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient (..), NodeClientDependencies (..), mkNodeClient)
import Language.Marlowe.Runtime.ChainSync.QueryServer (RunQueryServer (..))
import Language.Marlowe.Runtime.ChainSync.Server (RunChainSeekServer (..))
import Network.Channel (effectChannel, socketAsChannel)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), SockAddr, SocketOption (ReuseAddr), SocketType (..), bind,
                       close, defaultHints, getAddrInfo, listen, openSocket, setCloseOnExecIfNeeded, setSocketOption,
                       withFdSocket, withSocketsDo)
import Network.Socket.Address (accept)
import Network.TypedProtocol (runPeerWithDriver, startDState)
import Options (Options (..), getOptions)
import System.IO (hPrint, hPutStr, stderr)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  chainSeekAddr <- resolve port
  queryAddr <- resolve queryPort
  bracket (open chainSeekAddr) close \chainSeekSocket -> do
    bracket (open queryAddr) close \querySocket -> do
      socketIdVar <- newTVarIO @Int 0
      pool <- Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)
      genesisConfigResult <- runExceptT do
        hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
        (hash,) <$> withExceptT
          (const "failed to read byron genesis file")
          (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
      (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
      let genesisBlock = computeByronGenesisBlock (abstractHashToBytes hash) genesisConfig
      join $ atomically do
        let
          databaseQueries@DatabaseQueries{..} = hoistDatabaseQueries
            (either throwUsageError pure <=< Pool.use pool)
            (PostgreSQL.databaseQueries genesisBlock)
        NodeClient{..} <- mkNodeClient NodeClientDependencies
          { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
          , maxCost
          , costModel
          , getHeaderAtPoint
          , getIntersectionPoints
          }
        ChainSync{..} <- mkChainSync ChainSyncDependencies
          { getChanges
          , databaseQueries
          , persistRateLimit
          , genesisBlock
          , acceptRunChainSeekServer = do
              socketId <- atomically do
                modifyTVar socketIdVar (+1)
                readTVar socketIdVar
              (conn, _ :: SockAddr) <- accept chainSeekSocket
              let
                driver = mkDriver throwIO runtimeChainSeekCodec
                  $ effectChannel (logSend socketId) (logRecv socketId)
                  $ socketAsChannel conn
              pure $ RunChainSeekServer \server -> do
                let peer = chainSeekServerPeer Genesis server
                fst <$> (runPeerWithDriver driver peer (startDState driver) `finally` close conn)
          , acceptRunQueryServer = do
              (conn, _ :: SockAddr) <- accept querySocket
              let driver = mkDriver throwIO codecQuery $ socketAsChannel conn
              pure $ RunQueryServer \server -> do
                let peer = queryServerPeer server
                fst <$> (runPeerWithDriver driver peer (startDState driver) `finally` close conn)
          , queryLocalNodeState = queryNodeLocalState localNodeConnectInfo
          }
        pure $ runChainSync `concurrently_` runNodeClient
  where
    logSend i bytes =
      hPutStr stderr ("send[" <> show i <> "]: ") *> TL.hPutStrLn stderr (encodeBase16 bytes)
    logRecv i mbytes =
      hPutStr stderr ("recv[" <> show i <> "]: ") *> hPrint stderr (encodeBase16 <$> mbytes)
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
