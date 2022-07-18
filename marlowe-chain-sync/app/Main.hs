module Main where

import Cardano.Api (Block (Block), BlockHeader (..), BlockInMode (BlockInMode), ChainPoint (..), ChainTip (..),
                    ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..), NetworkId (..),
                    NetworkMagic (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad (guard)
import Control.Monad.STM (STM)
import Data.Functor (void)
import Language.Marlowe.Runtime.ChainSync.NodeClient (ChainSyncEvent (..), runNodeClient)
import Ouroboros.Network.Point (WithOrigin (..))

main :: IO ()
main = do
  tpoint <- newTVarIO ChainPointAtGenesis
  ttip <- newTVarIO ChainTipAtGenesis
  let
    runNodeClient' :: IO ()
    runNodeClient' =
      runNodeClient connectionInfo getHeaderAtPoint getIntersectionPoints $ atomically . \case
        RollForward (BlockInMode (Block (BlockHeader slotNo hash _) _) _) tip -> do
          writeTVar tpoint $ ChainPoint slotNo hash
          writeTVar ttip tip
        RollBackward point tip                                                -> do
          writeTVar tpoint point
          writeTVar ttip tip
      where
        connectionInfo = LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
          , localNodeNetworkId = Testnet $ NetworkMagic 1566
          , localNodeSocketPath = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
          }
        getHeaderAtPoint _ = pure Origin
        getIntersectionPoints _ _ = pure []

    awaitStatsChanges :: ChainPoint -> ChainTip -> STM (ChainPoint, ChainTip)
    awaitStatsChanges lastPoint lastTip = do
      point <- readTVar tpoint
      tip <- readTVar ttip
      guard $ (point, tip) /= (lastPoint, lastTip)
      pure (point, tip)

    writeStats :: ChainPoint -> ChainTip -> IO ()
    writeStats lastPoint lastTip = do
      (point, tip) <- atomically $ awaitStatsChanges lastPoint lastTip
      putStrLn $ "Current point: " <> show point <> "; Current tip: " <> show tip
      threadDelay 1_000_000
      writeStats point tip

  point <- readTVarIO tpoint
  tip <- readTVarIO ttip
  void $ concurrently runNodeClient' (writeStats point tip)
