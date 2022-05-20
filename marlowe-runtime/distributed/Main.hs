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
import Control.Distributed.Process (MonitorRef, Process, ProcessId, expect, getSelfPid, link, monitor, send, spawnLocal)
import Control.Distributed.Process.Internal.Types (Process (Process, unProcess))
import Control.Distributed.Process.Node (initRemoteTable, newLocalNode, runProcess)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary (..))
import Data.Function (fix)
import Data.Functor (void)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweChainEvent, MarloweChainPoint, MarloweChainTip)
import Network.Transport.TCP (TCPAddr (Unaddressable), createTransport, defaultTCPParameters)

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
  _ <- spawnLocalSupervised syncClientProcess
  fix \loop -> do
    msg :: ChainSyncMsg <- expect
    liftIO $ print msg
    case msg of
      ChainSyncDone -> pure ()
      _             -> loop

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
      (fmap (unProcess . send parent) . ChainSyncStart)
      (pure False)
      (unProcess . send parent . ChainSyncEvent)
      (unProcess $ send parent ChainSyncDone)

data ChainSyncMsg
  = ChainSyncStart (Maybe MarloweChainPoint) MarloweChainTip
  | ChainSyncEvent MarloweChainEvent
  | ChainSyncDone
  deriving (Generic, Typeable, Show)

instance Binary ChainSyncMsg

spawnLocalSupervised :: Process () -> Process (ProcessId, MonitorRef)
spawnLocalSupervised cp = do
  parent <- getSelfPid
  child <- spawnLocal do
    _ <- link parent
    cp
  monitorRef <- monitor child
  pure (child, monitorRef)
