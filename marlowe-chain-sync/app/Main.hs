module Main where

import Cardano.Api (CardanoMode, ConsensusModeParams (..), EpochSlots (..), LocalNodeConnectInfo (..))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), waitCatch, withAsync)
import Control.Concurrent.STM (atomically, newTVarIO, readTVar, writeTVar)
import Control.Exception (Exception (displayException))
import Control.Monad (forever)
import Data.Foldable (traverse_)
import Data.Functor (void)
import Data.Time (secondsToNominalDiffTime)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitRollback (..), GetHeaderAtPoint (..),
                                                    GetIntersectionPoints (..))
import Language.Marlowe.Runtime.ChainSync.NodeClient (NodeClient (..), NodeClientDependencies (..), mkNodeClient,
                                                      runNodeClient)
import Language.Marlowe.Runtime.ChainSync.Store (ChainStore (..), ChainStoreDependencies (..), mkChainStore)
import Options (Options (..), getOptions)
import Ouroboros.Network.Point (WithOrigin (..))
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  options <- getOptions "0.0.0.0"
  runChainSync options

runChainSync :: Options -> IO ()
runChainSync Options{..} = do
  tConnectionAttempts <- newTVarIO @Int 0
  let
    localNodeConnectInfo :: LocalNodeConnectInfo CardanoMode
    localNodeConnectInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = networkId
      , localNodeSocketPath = nodeSocket
      }
    getHeaderAtPoint :: GetHeaderAtPoint IO
    getHeaderAtPoint = GetHeaderAtPoint \_ -> pure Origin

    getIntersectionPoints :: GetIntersectionPoints IO
    getIntersectionPoints = GetIntersectionPoints \_ _ -> pure []

  (runNodeClient, runChainStore) <- atomically do
    NodeClient{..} <- mkNodeClient NodeClientDependencies
      { localNodeConnectInfo
      , getHeaderAtPoint
      , getIntersectionPoints
      }
    ChainStore{..} <- mkChainStore ChainStoreDependencies
      { commitRollback = CommitRollback \_ -> putStrLn "committing rollback"
      , commitBlocks = CommitBlocks \blocks -> putStrLn $ "saving " <> show (length blocks) <> " blocks"
      , rateLimit = secondsToNominalDiffTime 1
      , getChanges
      , clearChanges
      }
    pure (runNodeClient, runChainStore)

  let
    runNodeClientSupervised :: IO ()
    runNodeClientSupervised = forever $ withAsync runNodeClient \a -> do
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

    runChainStoreSupervised :: IO ()
    runChainStoreSupervised = withAsync runChainStore \a -> do
      result <- waitCatch a
      case result of
        Right _ -> pure ()
        Left exception -> do
          hPutStrLn stderr "Chain store exited unexpectedly, restarting."
          hPutStrLn stderr $ "Message: " <> displayException exception
          runChainStoreSupervised

  void $ runConcurrently $ traverse_ Concurrently
    [ runNodeClientSupervised
    , runChainStoreSupervised
    ]
