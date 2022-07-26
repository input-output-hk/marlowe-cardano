module Main where

import Cardano.Api (CardanoMode, ChainPoint (..), ChainTip (..), ConsensusModeParams (..), EpochSlots (..),
                    LocalNodeConnectInfo (..), SlotNo (..))
import Cardano.Api.Byron (toByronRequiresNetworkMagic)
import qualified Cardano.Chain.Genesis as Byron
import Cardano.Crypto (abstractHashToBytes, decodeAbstractHash)
import Control.Concurrent.STM (atomically)
import Control.Monad ((<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT), runExceptT, withExceptT)
import Data.Function (on)
import Data.String (IsString (fromString))
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (secondsToNominalDiffTime)
import qualified Hasql.Connection as Connection
import Hasql.Session (Session)
import qualified Hasql.Session as Session
import Language.Marlowe.Runtime.ChainSync (ChainSync (..), ChainSyncDependencies (..), mkChainSync)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitRollback (..), DatabaseQueries (..),
                                                    hoistDatabaseQueries)
import qualified Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL as PostgreSQL
import Options (Options (..), getOptions)

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
          , PostgreSQL.commitRollback
          ]
      , commitBlocks = mconcat
          [ CommitBlocks \blocks point tip -> do
              let
                percent = case (point, tip) of
                  (_, ChainTipAtGenesis)                             -> 1
                  (ChainPointAtGenesis, _)                           -> 0
                  (ChainPoint (SlotNo p) _, ChainTip (SlotNo t) _ _) -> on (/) fromIntegral p t
              liftIO $ putStrLn $ "saving " <> show (length blocks) <> " blocks (" <> case percent of
                1 -> "in sync)"
                _ -> show (floor (percent * 100 :: Double) :: Int) <> "%)"
          , PostgreSQL.commitBlocks
          ]
      , commitGenesisBlock = PostgreSQL.commitGenesisBlock
      , getHeaderAtPoint = PostgreSQL.getHeaderAtPoint
      , getIntersectionPoints = PostgreSQL.getIntersectionPoints
      , getGenesisBlock = PostgreSQL.getGenesisBlock
      }

    persistRateLimit = secondsToNominalDiffTime 1
