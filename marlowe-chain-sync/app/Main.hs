module Main where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVar)
import Control.Exception (bracket, bracketOnError)
import Control.Monad ((<=<))
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
import Language.Marlowe.Runtime.ChainSync.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.Genesis (computeByronGenesisBlock)
import Network.Channel (effectChannel, socketAsChannel)
import Network.Socket (AddrInfo (..), AddrInfoFlag (..), SockAddr, SocketOption (ReuseAddr), SocketType (..), bind,
                       close, defaultHints, getAddrInfo, listen, openSocket, setCloseOnExecIfNeeded, setSocketOption,
                       withFdSocket, withSocketsDo)
import Network.Socket.Address (accept)
import Options (Options (..), getOptions)
import System.IO (hPrint, hPutStr, stderr)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  addr <- resolve
  bracket (open addr) close \socket -> do
    socketIdVar <- newTVarIO @Int 0
    pool <- Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)
    genesisConfigResult <- runExceptT do
      hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
      (hash,) <$> withExceptT
        (const "failed to read byron genesis file")
        (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
    (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
    let genesisBlock = computeByronGenesisBlock (abstractHashToBytes hash) genesisConfig
    chainSync <- atomically $ mkChainSync ChainSyncDependencies
      { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
      , databaseQueries = hoistDatabaseQueries
          (either throwUsageError pure <=< Pool.use pool)
          (PostgreSQL.databaseQueries genesisBlock)
      , persistRateLimit
      , genesisBlock
      , acceptChannel = do
        socketId <- atomically do
          modifyTVar socketIdVar (+1)
          readTVar socketIdVar
        (conn, _ :: SockAddr) <- accept socket
        pure
          ( effectChannel (logSend socketId) (logRecv socketId) $ socketAsChannel conn
          , close conn
          )
      }
    runChainSync chainSync
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

    resolve = do
      let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
      head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)

    open addr = bracketOnError (openSocket addr) close \socket -> do
      setSocketOption socket ReuseAddr 1
      withFdSocket socket setCloseOnExecIfNeeded
      bind socket $ addrAddress addr
      listen socket 2048
      return socket
