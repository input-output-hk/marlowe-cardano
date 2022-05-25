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
module ChainSync.Logger where

import ChainSync.Client (ChainSyncMsg (..))
import Control.Distributed.Process (Closure, DiedReason (DiedNormal), Process, die, expect, say)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (when)
import Data.Binary (Binary)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweBlockNo (..), MarloweChainEvent (..),
                                             MarloweChainPoint (..), MarloweChainTip (..), MarloweSlotNo (..))

newtype ChainSyncLoggerConfig = ChainSyncLoggerConfig
  { syncLoggingFrequency :: Integer
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChainSyncLoggerConfig

chainSyncLogger :: ChainSyncLoggerConfig -> Process ()
chainSyncLogger ChainSyncLoggerConfig{..} = syncing 0 (10000 :: Integer)
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
          when (blocksSinceLastLog == syncLoggingFrequency - 1 || inSync) do
            say $ "Syncing (" <> show percentage <> "%; current slot: " <> show slot <> "; tip: " <> show tipNo <> ")"
          pure $ slot == tipNo
        ChainSyncDone -> do
          say "Exiting"
          pure False
      if inSync
        then do
          say "In sync, waiting for new blocks"
          synced
        else syncing ((blocksSinceLastLog + 1) `mod` syncLoggingFrequency) (deathCounter - 1)
    synced = do
      msg <- expect
      case msg of
        ChainSyncEvent (MarloweRollBackward point tip) -> do
          let pointNo = getPointNo point
          let tipNo = getTipNo tip
          say $ "Rolling back to:" <> show pointNo
          say $ "New tip: " <> show tipNo
          if pointNo == tipNo then
            synced
          else
            syncing 0 10000
        ChainSyncEvent (MarloweRollForward (MarloweBlockHeader (MarloweSlotNo slot) _ (MarloweBlockNo block)) _ _) -> do
          say $ "New block produced: slot: " <> show slot <> "; block: " <> show block
          synced
        ChainSyncDone -> do
          say "Exiting"
        _ -> synced
    getTipNo MarloweChainTipAtGenesis                = 0
    getTipNo (MarloweChainTip (MarloweSlotNo n) _ _) = n
    getPointNo MarloweChainPointAtGenesis              = 0
    getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n

remotable ['chainSyncLogger]

process :: ChainSyncLoggerConfig -> Closure (Process ())
process = $(mkClosure 'chainSyncLogger)
