module Main where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import Control.Concurrent.STM (atomically)
import Data.Time (secondsToNominalDiffTime)
import Language.Marlowe.Runtime.ChainSync (ChainSync (..), ChainSyncDependencies (..), mkChainSync)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitRollback (..), DatabaseQueries (..),
                                                    GetHeaderAtPoint (..), GetIntersectionPoints (..))
import Options (Options (..), getOptions)
import Ouroboros.Network.Point (WithOrigin (..))

main :: IO ()
main = run =<< getOptions "0.0.0.0"

run :: Options -> IO ()
run Options{..} = do
  chainSync <- atomically $ mkChainSync ChainSyncDependencies
    { localNodeConnectInfo
    , databaseQueries
    , persistRateLimit
    }
  runChainSync chainSync
  where
    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }

    databaseQueries :: DatabaseQueries IO
    databaseQueries = DatabaseQueries
      { commitRollback = CommitRollback \_ -> putStrLn "committing rollback"
      , commitBlocks = CommitBlocks \blocks -> putStrLn $ "saving " <> show (length blocks) <> " blocks"
      , getHeaderAtPoint = GetHeaderAtPoint \_ -> pure Origin
      , getIntersectionPoints = GetIntersectionPoints \_ _ -> pure []
      }

    persistRateLimit = secondsToNominalDiffTime 1
