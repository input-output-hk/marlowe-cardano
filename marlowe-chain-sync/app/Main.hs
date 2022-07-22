module Main where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.STM (atomically)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (secondsToNominalDiffTime)
import qualified Hasql.Connection as Connection
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSync (..), ChainSyncDependencies (..), mkChainSync)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitRollback (..), DatabaseQueries (..),
                                                    GetHeaderAtPoint (..), hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Options (Options (..), getOptions)
import Ouroboros.Network.Point (WithOrigin (..))

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = do
  connection <- either (error . maybe "Failed to connect to database" (unpack . decodeUtf8)) pure =<< Connection.acquire (fromString databaseUri)
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
        databaseQueries
    , persistRateLimit
    , genesisConfigHash = abstractHashToBytes hash
    , genesisConfig
    }
  runChainSync chainSync
  where
    throwQueryError (Session.QueryError _ _ err) = error $ show err
    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

    databaseQueries :: DatabaseQueries Session
    databaseQueries = DatabaseQueries
      { commitRollback = mconcat
          [ CommitRollback \point -> liftIO $ putStrLn $ "Rolling back to point " <> show point
          -- , PostgreSQL.commitRollback
          ]
      , commitBlocks = mconcat
          [ CommitBlocks \blocks -> liftIO $ putStrLn $ "saving " <> show (length blocks) <> " blocks"
          , PostgreSQL.commitBlocks
          ]
      , commitGenesisBlock = PostgreSQL.commitGenesisBlock
      , getHeaderAtPoint = GetHeaderAtPoint \_ -> pure Origin
      , getIntersectionPoints = PostgreSQL.getIntersectionPoints
      , getGenesisBlock = PostgreSQL.getGenesisBlock
      }

    persistRateLimit = secondsToNominalDiffTime 1
