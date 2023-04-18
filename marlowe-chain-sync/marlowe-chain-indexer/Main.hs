{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Main
  where

import Cardano.Api (CardanoMode, ConsensusModeParams(..), EpochSlots(..), LocalNodeConnectInfo(..))
import qualified Cardano.Api as Cardano
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.Component
import Control.Concurrent.Component.Probes (ProbeServerDependencies(..), probeServer)
import Control.Concurrent.Component.UnliftIO (convertComponent)
import Control.Exception (bracket)
import Control.Monad.Base (MonadBase)
import Control.Monad.Event.Class (MonadEvent(..), askBackendReaderT, localBackendReaderT)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, withExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With (MonadWith, WithException, stateThreadingGeneralWith)
import Data.Aeson (eitherDecodeFileStrict)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(GeneralAllocated))
import Data.String (IsString(fromString))
import Data.Text (unpack)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import Data.Time (secondsToNominalDiffTime)
import Data.UUID.V4 (nextRandom)
import qualified Hasql.Pool as Pool
import Language.Marlowe.Runtime.ChainIndexer (ChainIndexerDependencies(..), chainIndexer)
import qualified Language.Marlowe.Runtime.ChainIndexer.Database.PostgreSQL as PostgreSQL
import Language.Marlowe.Runtime.ChainIndexer.Genesis (computeGenesisBlock)
import Logging (RootSelector(..), getRootSelectorConfig)
import Observe.Event (EventBackend, injectSelector)
import Observe.Event.Backend (hoistEventBackend)
import Observe.Event.Component (LoggerDependencies(..), withLogger)
import Options (Options(..), getOptions)
import System.IO (stderr)
import UnliftIO (MonadIO, MonadUnliftIO, liftIO)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = do
  genesisConfigResult <- runExceptT do
    hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
    (hash,) <$> withExceptT
      (mappend "failed to read byron genesis file: " . T.pack . show)
      (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
  (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
  shelleyGenesis <- either error id <$> eitherDecodeFileStrict shelleyGenesisFile
  let
    genesisBlock = computeGenesisBlock (abstractHashToBytes hash) genesisConfig shelleyGenesis
    appComponent = proc pool -> do
      probes <- convertComponent chainIndexer -< ChainIndexerDependencies
        { connectToLocalNode = Cardano.connectToLocalNode localNodeConnectInfo
        , databaseQueries = PostgreSQL.databaseQueries pool genesisBlock
        , persistRateLimit
        , genesisBlock
        , maxCost
        , costModel
        }
      probeServer -< ProbeServerDependencies
        { probes
        , port = httpPort
        }

  bracket (Pool.acquire 100 (Just 5000000) (fromString databaseUri)) Pool.release
    $ runComponent_
    $ withLogger loggerDependencies runAppM appComponent
  where
    loggerDependencies = LoggerDependencies
      { configFilePath = logConfigFile
      , getSelectorConfig = getRootSelectorConfig
      , newRef = nextRandom
      , writeText = TL.hPutStr stderr
      , injectConfigWatcherSelector = injectSelector ConfigWatcher
      }

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- FIXME read from config - what is the appropriate value?
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

    persistRateLimit = secondsToNominalDiffTime 1

runAppM :: EventBackend IO r RootSelector -> AppM r a -> IO a
runAppM eventBackend = flip runReaderT (hoistEventBackend liftIO eventBackend) . unAppM

newtype AppM r a = AppM
  { unAppM :: ReaderT (EventBackend (AppM r) r RootSelector) IO a
  } deriving newtype (Functor, Applicative, Monad, MonadBase IO, MonadBaseControl IO, MonadIO, MonadUnliftIO)

instance MonadWith (AppM r) where
  type WithException (AppM r) = WithException IO
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (AppM r) (WithException IO) releaseReturn b a
    -> (a -> AppM r b)
    -> AppM r (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = AppM . ReaderT $ \r -> do
    let
      allocA' :: (forall x. IO x -> IO x) -> IO (GeneralAllocated IO (WithException IO) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. AppM r x -> AppM r x
          restore' mx = AppM . ReaderT $ restore . (runReaderT . unAppM) mx
        GeneralAllocated a releaseA <- (runReaderT . unAppM) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unAppM) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unAppM) r . go)

instance MonadEvent r RootSelector (AppM r) where
  askBackend = askBackendReaderT AppM id
  localBackend = localBackendReaderT AppM unAppM id
