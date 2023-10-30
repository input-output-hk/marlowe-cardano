{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Cardano.Api (
  AnyCardanoEra (..),
  CardanoMode,
  ConsensusMode (..),
  ConsensusModeIsMultiEra (..),
  ConsensusModeParams (..),
  EpochSlots (..),
  EraInMode (..),
  File (..),
  GenesisParameters (..),
  LocalNodeConnectInfo (..),
  QueryInEra (..),
  QueryInMode (..),
  QueryInShelleyBasedEra (..),
  ShelleyBasedEra (..),
  queryNodeLocalState,
  toEraInMode,
 )
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies (..), probeServer)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Exception (bracket)
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import Data.Time (secondsToNominalDiffTime)
import Data.Version (showVersion)
import qualified Database.PostgreSQL.LibPQ as PQ
import Hasql.Connection (withLibPQConnection)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies (..), chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainIndexer.Genesis (computeGenesisBlock)
import Logging (renderRootSelectorOTel)
import OpenTelemetry.Trace
import Options (Options (..), getOptions)
import Paths_marlowe_chain_sync (version)
import UnliftIO (liftIO, throwIO)

main :: IO ()
main = run =<< getOptions (showVersion version)

run :: Options -> IO ()
run Options{..} = do
  genesisConfigResult <- runExceptT do
    hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
    (hash,)
      <$> withExceptT
        (mappend "failed to read byron genesis file: " . T.pack . show)
        (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
  (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
  shelleyGenesis <- either error id <$> eitherDecodeFileStrict shelleyGenesisFile
  Right (AnyCardanoEra era) <-
    queryNodeLocalState localNodeConnectInfo Nothing $ QueryCurrentEra CardanoModeIsMultiEra
  eraInMode <- case toEraInMode era CardanoMode of
    Nothing -> fail $ "cannot convert " <> show era <> " to era in mode"
    Just eraInMode -> pure eraInMode
  shelleyBasedEra <- case eraInMode of
    ByronEraInCardanoMode -> fail "Cannot query shelley in byron era"
    ShelleyEraInCardanoMode -> pure ShelleyBasedEraShelley
    AllegraEraInCardanoMode -> pure ShelleyBasedEraAllegra
    MaryEraInCardanoMode -> pure ShelleyBasedEraMary
    AlonzoEraInCardanoMode -> pure ShelleyBasedEraAlonzo
    BabbageEraInCardanoMode -> pure ShelleyBasedEraBabbage
    ConwayEraInCardanoMode -> pure ShelleyBasedEraConway
  Right (Right GenesisParameters{..}) <-
    queryNodeLocalState localNodeConnectInfo Nothing $
      QueryInEra eraInMode $
        QueryInShelleyBasedEra shelleyBasedEra QueryGenesisParameters
  let securityParameter = fromIntegral protocolParamSecurity
  let genesisBlock = computeGenesisBlock (abstractHashToBytes hash) genesisConfig shelleyGenesis
      appComponent = proc pool -> do
        probes <-
          chainIndexer
            -<
              ChainIndexerDependencies
                { connectToLocalNode = liftIO . Cardano.connectToLocalNode localNodeConnectInfo
                , databaseQueries = PostgreSQL.databaseQueries securityParameter pool genesisBlock
                , persistRateLimit
                , genesisBlock
                , maxCost
                , costModel
                }
        probeServer
          -<
            ProbeServerDependencies
              { probes
              , port = httpPort
              }

  bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release \pool -> do
    (dbName, dbUser, dbHost, dbPort) <-
      either throwIO pure =<< Pool.use pool do
        connection <- ask
        liftIO $ withLibPQConnection connection \conn ->
          (,,,)
            <$> PQ.db conn
            <*> PQ.user conn
            <*> PQ.host conn
            <*> PQ.port conn
    runAppMTraced instrumentationLibrary (renderRootSelectorOTel dbName dbUser dbHost dbPort) $
      runComponent_ appComponent pool
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-chain-indexer"
        , libraryVersion = T.pack $ showVersion version
        }

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo =
      LocalNodeConnectInfo
        { -- FIXME read from config - what is the appropriate value?
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId = networkId
        , localNodeSocketPath = File nodeSocket
        }

    persistRateLimit = secondsToNominalDiffTime 1
