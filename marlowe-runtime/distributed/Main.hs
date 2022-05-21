{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
module Main where

import Cardano.Api
import Control.Distributed.Process (DiedReason (DiedNormal), MonitorRef, Process, ProcessId,
                                    ProcessInfo (ProcessInfo, infoRegisteredNames), ProcessMonitorNotification (..),
                                    die, expect, getProcessInfo, getSelfPid, liftIO, link, match, matchIf, monitor,
                                    nsend, receiveWait, register, reregister, say, send, spawnLocal)
import Control.Distributed.Process.Internal.Primitives (SayMessage (..))
import Control.Distributed.Process.Internal.Types (Process (Process, unProcess))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Monad (forever, when)
import Data.Binary (Binary (..))
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Traversable (forM)
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
  _ <- spawnLocalLink $ supervisor "app"
    [ ("chain-sync:client", syncClientProcess, False)
    , ("chain-sync:logger", chainSyncLogger, False)
    , ("logger", appLogger, True)
    ]
  forever $ receiveWait
    [ match \(msg :: ChainSyncMsg) -> do
        nsend "chain-sync:logger" msg
        nsend "history:digest" msg
    ]

data ChainSyncMsg
  = ChainSyncStart MarloweChainPoint MarloweChainTip
  | ChainSyncEvent MarloweChainEvent
  | ChainSyncDone
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncMsg

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

blockLoggingFrequency :: Integer
blockLoggingFrequency = 4000

chainSyncLogger :: Process ()
chainSyncLogger = syncing (0 :: Integer) (10000 :: Integer)
  where
    syncing blocksSinceLastLog deathCounter = do
      when (deathCounter == 0) $ die DiedNormal -- die for fun
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
          when (blocksSinceLastLog == blockLoggingFrequency - 1 || inSync) do
            say $ "Syncing (" <> show percentage <> "%; current slot: " <> show slot <> "; tip: " <> show tipNo <> ")"
          pure $ slot == tipNo
        ChainSyncDone -> do
          say "Exiting"
          pure False
      if inSync
        then do
          say "In sync, waiting for new blocks"
          synced
        else syncing ((blocksSinceLastLog + 1) `mod` blockLoggingFrequency) (deathCounter - 1)
    synced = do
      msg <- expect
      case msg of
        ChainSyncEvent (MarloweRollBackward point tip) -> do
          say $ "Rolling back to:" <> show point
          say $ "New tip: " <> show tip
          syncing 0 10000
        ChainSyncDone -> do
          say "Exiting"
        _ -> synced
    getTipNo MarloweChainTipAtGenesis                = 0
    getTipNo (MarloweChainTip (MarloweSlotNo n) _ _) = n
    getPointNo MarloweChainPointAtGenesis              = 0
    getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n

appLogger :: Process ()
appLogger = forever do
  SayMessage{..} <- expect
  getProcessInfo sayProcess >>= mapM_ \ProcessInfo{..} -> do
    let
      label = case infoRegisteredNames of
        name : _ -> name
        _        -> show sayProcess
    let timestamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) sayTime
    liftIO $ putStrLn $ "[" <> timestamp <> "] [" <> label <> "] " <> sayMessage

supervisor :: String -> [(String, Process (), Bool)] -> Process ()
supervisor selfName children = do
  register (selfName <> ":supervisor") =<< getSelfPid
  childRefs <- forM children \(name, process, replace) -> do
    (pid, ref) <- spawnLocalSupervised process
    if replace then
      reregister name pid
    else
      register name pid
    pure (name, ref, process)
  loop childRefs
    where
      loop childRefs = receiveWait $ flip fmap childRefs \(name, childRef, process) -> matchIf
        (\(ProcessMonitorNotification ref _ _) -> ref == childRef)
        (\(ProcessMonitorNotification _ _ reason) -> do
            say $ "Child process " <> name <> " terminated, restarting"
            say $ "Message was " <> show reason
            (pid, ref) <- spawnLocalSupervised process
            register name pid
            loop $ fmap (\(n, r, p) -> if n == name then (n, ref, process) else (n, r, p)) childRefs
        )

spawnLocalSupervised :: Process () -> Process (ProcessId, MonitorRef)
spawnLocalSupervised cp = do
  parent <- getSelfPid
  child <- spawnLocal do
    _ <- link parent
    cp
  monitorRef <- monitor child
  pure (child, monitorRef)

spawnLocalLink :: Process () -> Process ProcessId
spawnLocalLink cp = do
  child <- spawnLocal cp
  link child
  pure child
