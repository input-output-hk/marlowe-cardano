{-# LANGUAGE GADTs #-}

module Main
  where

import Cardano.Api (CardanoMode, ConsensusModeParams(..), EpochSlots(..), LocalNodeConnectInfo(..))
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Arrow (arr)
import Control.Category ((<<<))
import Control.Concurrent.Component
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Data.Aeson (eitherDecodeFileStrict)
import Data.String (IsString(fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Time (secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import Hasql.Pool (UsageError(..))
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import Language.Marlowe.Runtime.ChainIndexer.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainIndexer.Genesis (computeGenesisBlock)
import Logging (RootSelector(..), getRootSelectorConfig)
import Observe.Event.Component (LoggerDependencies(..), logger)
import Observe.Event.Explicit (injectSelector, narrowEventBackend)
import Options (Options(..), getOptions)
import System.IO (stderr)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = do
  pool <- Pool.acquire 100 (Just 5000000) (fromString databaseUri)
  genesisConfigResult <- runExceptT do
    hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
    (hash,) <$> withExceptT
      (mappend "failed to read byron genesis file: " . T.pack . show)
      (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
  (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
  shelleyGenesis <- either error id <$> eitherDecodeFileStrict shelleyGenesisFile
  let genesisBlock = computeGenesisBlock (abstractHashToBytes hash) genesisConfig shelleyGenesis
  let
    chainIndexerDependencies eventBackend = ChainIndexerDependencies
      { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
      , databaseQueries = hoistDatabaseQueries
          (either throwUsageError pure <=< Pool.use pool)
          (PostgreSQL.databaseQueries genesisBlock)
      , persistRateLimit
      , genesisBlock
      , maxCost
      , costModel
      , eventBackend = narrowEventBackend (injectSelector App) eventBackend
      , httpPort
      }
    loggerDependencies = LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = injectSelector ConfigWatcher
      }
    appComponent = chainIndexer <<< arr chainIndexerDependencies <<< logger
  runComponent_ appComponent loggerDependencies
  where
    throwUsageError (ConnectionUsageError err)                       = error $ show err
    throwUsageError (SessionUsageError (Session.QueryError _ _ err)) = error $ show err
    throwUsageError AcquisitionTimeoutUsageError                     = error "hasql-timeout"

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- FIXME read from config - what is the appropriate value?
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

    persistRateLimit = secondsToNominalDiffTime 1
