{-# LANGUAGE FlexibleContexts #-}
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
import Colog (logDebug)
import qualified Colog
import Control.Category ((<<<))
import Control.Concurrent.STM (atomically, modifyTVar, newTVarIO, readTVar)
import Control.Exception (throwIO)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Data.ByteString.Lazy.Base16 (encodeBase16)
import Data.String (IsString(fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (secondsToNominalDiffTime)
import Hasql.Pool (UsageError(..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.CLI.Option.Colog (mkLogger)
import Language.Marlowe.Runtime.ChainSync (ChainSync(..), ChainSyncDependencies(..), mkChainSync)
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis(..), runtimeChainSeekCodec)
import Language.Marlowe.Runtime.ChainSync.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainSync.Genesis (computeByronGenesisBlock)
import Language.Marlowe.Runtime.ChainSync.JobServer (RunJobServer(..))
import Language.Marlowe.Runtime.ChainSync.QueryServer (RunQueryServer(..))
import Language.Marlowe.Runtime.ChainSync.Server (RunChainSeekServer(..))
import Language.Marlowe.Runtime.Logging.Colog.LogIO (finallyLogIO, runLogIO, throwLogIO)
import Language.Marlowe.Runtime.Logging.Colog.LogIO.Network (ProtocolName(ProtocolName), withServerSocket)
import Network.Channel (effectChannel, hoistChannel, socketAsChannel)
import Network.Protocol.ChainSeek.Server (chainSeekServerPeer)
import Network.Protocol.Driver (hoistDriver, mkDriver)
import Network.Protocol.Job.Codec (codecJob)
import Network.Protocol.Job.Server (jobServerPeer)
import Network.Protocol.Query.Codec (codecQuery)
import Network.Protocol.Query.Server (queryServerPeer)
import Network.Socket
  (AddrInfo(..), AddrInfoFlag(..), SockAddr, SocketType(..), close, defaultHints, getAddrInfo, withSocketsDo)
import Network.Socket.Address (accept)
import Network.TypedProtocol (runPeerWithDriver, startDState)
import Options (Options(..), getOptions)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = withSocketsDo do
  chainSeekAddr <- resolve port
  queryAddr <- resolve queryPort
  commandAddr <- resolve commandPort
  Colog.withBackgroundLogger Colog.defCapacity rootLogAction \logAction -> do
    runLogIO logAction $ withServerSocket (ProtocolName "ChainSeek") chainSeekAddr \chainSeekSocket -> do
      withServerSocket (ProtocolName "Query") queryAddr \querySocket -> do
        withServerSocket (ProtocolName "Job") commandAddr \commandSocket -> do
          chainSync <- liftIO do
            socketIdVar <- newTVarIO @Int 0
            pool <- Pool.acquire (100, secondsToNominalDiffTime 5, fromString databaseUri)
            genesisConfigResult <- runExceptT do
              hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
              (hash,) <$> withExceptT
                (const "failed to read byron genesis file")
                (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
            (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
            let genesisBlock = computeByronGenesisBlock (abstractHashToBytes hash) genesisConfig
            atomically $ mkChainSync ChainSyncDependencies
              { connectToLocalNode = liftIO <$> Cardano.connectToLocalNode localNodeConnectInfo
              , databaseQueries = hoistDatabaseQueries
                  (liftIO <<< either throwUsageError pure <=< Pool.use pool)
                  (PostgreSQL.databaseQueries genesisBlock)
              , persistRateLimit
              , genesisBlock
                , acceptRunChainSeekServer = do
                    (conn, driver) <- liftIO $ do
                      socketId <- atomically do
                        modifyTVar socketIdVar (+1)
                        readTVar socketIdVar
                      (conn, _ :: SockAddr) <- accept chainSeekSocket
                      pure . (conn,) $ mkDriver (throwLogIO "ChainSeek protocol execution error") runtimeChainSeekCodec
                          $ effectChannel (logSend socketId) (logRecv socketId)
                          $ hoistChannel liftIO (socketAsChannel conn)
                    pure $ RunChainSeekServer \server -> do
                      let peer = chainSeekServerPeer Genesis server
                      fst <$> (runPeerWithDriver driver peer (startDState driver) `finallyLogIO` (liftIO $ close conn))
              , acceptRunQueryServer = do
                  (conn, _ :: SockAddr) <- liftIO $ accept querySocket
                  let
                    driver = mkDriver
                      (throwLogIO "Query protocol execution error")
                      codecQuery
                      $ hoistChannel liftIO (socketAsChannel conn)
                  pure $ RunQueryServer \server -> do
                    let peer = queryServerPeer server
                    fst <$> runPeerWithDriver driver peer (startDState driver)
              , acceptRunJobServer = do
                  (conn, _ :: SockAddr) <- liftIO $ accept commandSocket
                  let driver = hoistDriver liftIO $ mkDriver throwIO codecJob $ socketAsChannel conn
                  pure $ RunJobServer \server -> do
                    let peer = jobServerPeer server
                    fst <$> runPeerWithDriver driver peer (startDState driver)
              , queryLocalNodeState = \point query -> liftIO (queryNodeLocalState localNodeConnectInfo point query)
              , submitTxToNodeLocal = \era tx -> liftIO $ Cardano.submitTxToNodeLocal localNodeConnectInfo $ TxInMode tx case era of
                  ByronEra -> ByronEraInCardanoMode
                  ShelleyEra -> ShelleyEraInCardanoMode
                  AllegraEra -> AllegraEraInCardanoMode
                  MaryEra -> MaryEraInCardanoMode
                  AlonzoEra -> AlonzoEraInCardanoMode
                  BabbageEra -> BabbageEraInCardanoMode
              , maxCost
              , costModel
              }
          runChainSync chainSync
  where
    rootLogAction = mkLogger verbosity

    logSend i bytes =
      logDebug $ "send[" <> T.pack (show i) <> "]: " <> TL.toStrict (encodeBase16 bytes)
    logRecv i mbytes =
      logDebug $ "recv[" <> T.pack (show i) <> "]: " <> (T.pack . show $ encodeBase16 <$> mbytes)

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

