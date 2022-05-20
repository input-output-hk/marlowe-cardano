{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
module Main where

import Cardano.Api
import Control.Distributed.Process (DiedReason (..), MonitorRef, Process, ProcessId, ProcessMonitorNotification (..),
                                    expect, getSelfPid, link, match, matchIf, monitor, nsend, receiveWait, register,
                                    reregister, say, send, spawnLocal)
import Control.Distributed.Process.Internal.Types (Process (Process, unProcess))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Monad (when)
import Data.Binary (Binary (..))
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainEvent (..), MarloweChainPoint (..),
                                             MarloweChainTip (..), MarloweSlotNo (..))
import Network.Transport.TCP (TCPAddr (..), createTransport, defaultTCPParameters)

main :: IO ()
main = do
  Right transport <- createTransport Unaddressable defaultTCPParameters
  node <- newLocalNode transport initRemoteTable
  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = Testnet $ NetworkMagic 1566
      , localNodeSocketPath = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
      }
  void $ runProcess node $ app connectionInfo

app :: LocalNodeConnectInfo CardanoMode -> Process ()
app connectionInfo = do
  self <- getSelfPid
  Right syncClientProcess <- chainSyncClient connectionInfo [] self
  (syncClient, clientMonitor) <- spawnLocalSupervised syncClientProcess
  register "chain-sync-client" syncClient
  (logger, loggerMonitor) <- spawnLocalSupervised chainSyncLogger
  register "chain-sync-logger" logger
  loop clientMonitor loggerMonitor syncClientProcess
    where
      loop clientMonitor loggerMonitor syncClientProcess = receiveWait
        [ match \(msg :: ChainSyncMsg) -> do
            nsend "chain-sync-logger" msg
            loop clientMonitor loggerMonitor syncClientProcess
        , matchIf (\(ProcessMonitorNotification ref _ _) -> ref == clientMonitor)
          \(ProcessMonitorNotification _ _ reason) -> case reason of
              DiedNormal -> do
                say "Chain sync client exited normally, shutting down..."
                pure ()
              _ -> do
                say "Chain sync client exited with an exception, restarting..."
                say $ "Message: " <> show reason
                (syncClient, clientMonitor') <- spawnLocalSupervised syncClientProcess
                reregister "chain-sync-client" syncClient
                loop clientMonitor' loggerMonitor syncClientProcess
        , matchIf (\(ProcessMonitorNotification ref _ _) -> ref == loggerMonitor)
          \(ProcessMonitorNotification _ _ reason) -> do
              say "Chain sync logger exited with an exception, restarting..."
              say $ "Message: " <> show reason
              (logger, loggerMonitor') <- spawnLocalSupervised chainSyncLogger
              reregister "chain-sync-logger" logger
              loop clientMonitor loggerMonitor' syncClientProcess
        ]

chainSyncClient
  :: LocalNodeConnectInfo CardanoMode
  -> [ChainPoint]
  -> ProcessId
  -> Process (Either String (Process ()))
chainSyncClient connectionInfo startingPoints parent = do
  runMarloweChainSyncClient connectionInfo \genesisParams -> Process do
    marloweChainSyncClient
      startingPoints
      genesisParams
      (fmap (unProcess . send parent) . ChainSyncStart . fromMaybe MarloweChainPointAtGenesis)
      (pure False)
      (unProcess . send parent . ChainSyncEvent)
      (unProcess $ send parent ChainSyncDone)

chainSyncLogger :: Process ()
chainSyncLogger = syncing (0 :: Integer)
  where
    syncing blocksSinceLastLog = do
      msg <- expect
      inSync <- case msg of
        ChainSyncStart point tip -> do
          let pointNo = getPointNo point
          let tipNo = getTipNo tip
          say $ "Chain sync client started from slot " <> show pointNo
          say $ "Tip of local node: " <> show tipNo
          pure $ tipNo == pointNo
        ChainSyncEvent (MarloweRollBackward point tip) -> do
          let pointNo = getPointNo point
          let tipNo = getTipNo tip
          say $ "Rolling back to slot: " <> show pointNo
          say $ "New tip: " <> show tipNo
          pure $ tipNo == pointNo
        ChainSyncEvent (MarloweRollForward (MarloweBlockHeader (MarloweSlotNo slot) _ _) _ tip) -> do
          let tipNo = getTipNo tip
          let inSync = tipNo == slot
          let percentage = (100 * slot) `div` tipNo
          when (blocksSinceLastLog == 1999 || inSync) do
            say $ "Syncing (" <> show percentage <> "%; current slot: " <> show slot <> "; tip: " <> show tipNo <> ")"
          pure $ slot == tipNo
        ChainSyncDone -> do
          say "Exiting"
          pure False
      if inSync
        then do
          say "In sync, waiting for new blocks"
          synced
        else syncing ((blocksSinceLastLog + 1) `mod` 2000)
    synced = do
      msg <- expect
      case msg of
        ChainSyncEvent (MarloweRollBackward point tip) -> do
          say $ "Rolling back to:" <> show point
          say $ "New tip: " <> show tip
          syncing 0
        ChainSyncDone -> do
          say "Exiting"
        _ -> synced
    getTipNo MarloweChainTipAtGenesis                = 0
    getTipNo (MarloweChainTip (MarloweSlotNo n) _ _) = n
    getPointNo MarloweChainPointAtGenesis              = 0
    getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n


data ChainSyncMsg
  = ChainSyncStart MarloweChainPoint MarloweChainTip
  | ChainSyncEvent MarloweChainEvent
  | ChainSyncDone
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncMsg

spawnLocalSupervised :: Process () -> Process (ProcessId, MonitorRef)
spawnLocalSupervised cp = do
  parent <- getSelfPid
  child <- spawnLocal do
    _ <- link parent
    cp
  monitorRef <- monitor child
  pure (child, monitorRef)
