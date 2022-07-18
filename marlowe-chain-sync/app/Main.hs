module Main where

import Cardano.Api (Block (Block), BlockHeader (..), BlockInMode (BlockInMode), ChainPoint (..), ChainTip (..),
                    ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (concurrently, waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (Exception (displayException))
import Control.Monad (guard)
import Control.Monad.STM (STM)
import Data.Functor (void)
import Language.Marlowe.Runtime.ChainSync.NodeClient (ChainSyncEvent (..), runNodeClient)
import Options (Options (..), getOptions)
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options <- getOptions "0.0.0.0"
  runChainSync options

runChainSync :: Options -> IO ()
runChainSync Options{..} = do
  tpoint <- newTVarIO ChainPointAtGenesis
  ttip <- newTVarIO ChainTipAtGenesis
  tConnectionAttempts <- newTVarIO @Int 0
  let
    runNodeClientSupervised :: IO ()
    runNodeClientSupervised = withAsync runNodeClient' \a -> do
      result <- waitCatch a
      case result of
        Right _ -> pure ()
        Left exception -> do
          connectionAttempts <- atomically do
            connectionAttempts <- readTVar tConnectionAttempts
            writeTVar tConnectionAttempts $ connectionAttempts + 1
            pure connectionAttempts
          let
            timeoutSeconds :: Int
            timeoutSeconds = min 15 $ floor @Double $ exp $ fromIntegral connectionAttempts
          let
            msg = if connectionAttempts == 0
              then "Connection to local node lost."
              else "Connection attempt #" <> show connectionAttempts <> " failed."
          hPutStrLn stderr $ msg <> " Attempting to reconnect in " <> show timeoutSeconds <> "s."
          hPutStrLn stderr $ "Message: " <> displayException exception
          threadDelay $ 1_000_000 * timeoutSeconds
          runNodeClientSupervised

    runNodeClient' :: IO ()
    runNodeClient' =
      runNodeClient connectionInfo getHeaderAtPoint getIntersectionPoints \event -> atomically do
        writeTVar tConnectionAttempts 0
        case event of
          RollForward (BlockInMode (Block (BlockHeader slotNo hash _) _) _) tip -> do
            writeTVar tpoint $ ChainPoint slotNo hash
            writeTVar ttip tip
          RollBackward point tip                                                -> do
            writeTVar tpoint point
            writeTVar ttip tip
      where
        connectionInfo = LocalNodeConnectInfo
          { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
          , localNodeNetworkId = networkId
          , localNodeSocketPath = nodeSocket
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
  void $ concurrently runNodeClientSupervised (writeStats point tip)
