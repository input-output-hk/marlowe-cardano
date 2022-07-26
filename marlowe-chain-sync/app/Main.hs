module Main where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.STM (atomically)
import Control.Monad ((<=<))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (secondsToNominalDiffTime)
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSync (..), ChainSyncDependencies (..), mkChainSync)
import Language.Marlowe.Runtime.ChainSync.Database (hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Options (Options (..), getOptions)

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = do
  connectionResult <- Connection.acquire (fromString databaseUri)
  connection <- either
    (fail . maybe "Failed to connect to database" (unpack . decodeUtf8))
    pure
    connectionResult
  genesisConfigResult <- runExceptT do
    hash <- ExceptT $ pure $ decodeAbstractHash genesisConfigHash
    (hash,) <$> withExceptT
      (const "failed to read byron genesis file")
      (Byron.mkConfigFromFile (toByronRequiresNetworkMagic networkId) genesisConfigFile hash)
  (hash, genesisConfig) <- either (fail . unpack) pure genesisConfigResult
  chainSync <- atomically $ mkChainSync ChainSyncDependencies
    { localNodeConnectInfo
    , databaseQueries = hoistDatabaseQueries
        (either throwQueryError pure <=< flip Session.run connection)
        PostgreSQL.databaseQueries
    , persistRateLimit
    , genesisConfigHash = abstractHashToBytes hash
    , genesisConfig
    }
  runChainSync chainSync
  where
    throwQueryError (Session.QueryError _ _ err) = error $ show err

    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      -- FIXME read from config - what is the appropriate value?
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

    persistRateLimit = secondsToNominalDiffTime 1
