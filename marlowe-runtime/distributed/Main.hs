{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module Main where

import Cardano.Api
import Cardano.Api.Shelley (Hash (HeaderHash))
import Control.Distributed.Process (DiedReason (DiedNormal), Process, ProcessId,
                                    ProcessInfo (ProcessInfo, infoRegisteredNames), RemoteTable, die, expect,
                                    getProcessInfo, getSelfPid, liftIO, link, match, nsend, receiveWait, reregister,
                                    say, send, spawnLocal)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras.Time (Delay (..), TimeInterval, minutes)
import Control.Distributed.Process.Internal.Primitives (SayMessage (..))
import Control.Distributed.Process.Internal.Types (Process (Process, unProcess))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..), ShutdownMode (..), restartOne)
import qualified Control.Distributed.Process.Supervisor as Supervisor
import Control.Monad (forever, when)
import Data.Binary (Binary (..))
import Data.Functor (void)
import Data.Maybe (fromMaybe)
import Data.Time.Format (defaultTimeLocale, formatTime, iso8601DateFormat)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockHeaderHash (MarloweBlockHeaderHash),
                                             MarloweChainEvent (..), MarloweChainPoint (..), MarloweChainTip (..),
                                             MarloweSlotNo (..))
import Network.Transport.TCP (TCPAddr (..), createTransport, defaultTCPParameters)

appLogger :: Process ()
appLogger = forever do
  SayMessage{..} <- expect
  getProcessInfo sayProcess >>= mapM_ \ProcessInfo{..} -> do
    let
      label = case infoRegisteredNames of
        name : _ -> name
        _        -> show sayProcess
    let timestamp = formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S")) sayTime
    liftIO $ putStrLn $ "[" <> label <> "] [" <> timestamp <> "] " <> sayMessage

spawnLocalLink :: Process () -> Process ProcessId
spawnLocalLink cp = do
  child <- spawnLocal cp
  link child
  pure child

data ChainSyncMsg
  = ChainSyncStart MarloweChainPoint MarloweChainTip
  | ChainSyncEvent MarloweChainEvent
  | ChainSyncDone
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncMsg

chainSyncClient :: ([MarloweChainPoint], ProcessId) -> Process ()
chainSyncClient (startingPoints, parent) = do
  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = Testnet $ NetworkMagic 1566
      , localNodeSocketPath = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
      }
    toChainPoint MarloweChainPointAtGenesis = ChainPointAtGenesis
    toChainPoint (MarloweChainPoint (MarloweSlotNo slot) (MarloweBlockHeaderHash hash)) =
      ChainPoint (SlotNo slot) (HeaderHash hash)
  either fail id =<< runMarloweChainSyncClient connectionInfo \genesisParams -> Process do
    marloweChainSyncClient
      (toChainPoint <$> startingPoints)
      genesisParams
      (fmap (unProcess . send parent) . ChainSyncStart . fromMaybe MarloweChainPointAtGenesis)
      (pure False)
      (unProcess . send parent . ChainSyncEvent)
      (unProcess $ send parent ChainSyncDone)

blockLoggingFrequency :: Integer
blockLoggingFrequency = 4000

chainSyncLogger :: () -> Process ()
chainSyncLogger _ = syncing (0 :: Integer) (10000 :: Integer)
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

remotable ['chainSyncLogger, 'chainSyncClient]

remoteTable :: RemoteTable
remoteTable = Main.__remoteTable initRemoteTable

app :: Process ()
app = do
  self <- getSelfPid
  supervisor <- Supervisor.start restartOne ParallelShutdown
    [ worker "chain-sync.client" Intrinsic Nothing (StopTimeout (Delay $ minutes 1)) $ RunClosure $ $(mkClosure 'chainSyncClient) ([] :: [MarloweChainPoint], self)
    , worker "chain-sync.logger" Intrinsic Nothing StopImmediately $ RunClosure $ $(mkClosure 'chainSyncLogger) ()
    ]
  link supervisor
  reregister "logger" =<< spawnLocalLink appLogger
  forever $ receiveWait
    [ match \(msg :: ChainSyncMsg) -> do
        nsend "chain-sync.logger" msg
        nsend "history.digest" msg
    ]

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

main :: IO ()
main = do
  Right transport <- createTransport Unaddressable defaultTCPParameters
  node <- newLocalNode transport remoteTable
  void $ runProcess node app
