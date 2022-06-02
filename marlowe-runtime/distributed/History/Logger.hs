{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module History.Logger where

import ChainSync.Client (ChainSyncMsg (..))
import Control.Distributed.Process (Closure, Process, match, receiveWait, say)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (when)
import Data.Binary (Binary)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainEvent (..), MarloweChainPoint (..),
                                             MarloweChainTip (..), MarloweSlotNo (..))
import Language.Marlowe.Runtime.History.Types (AppTxOutRef (..), Event (..), HistoryEvent (..))

newtype HistoryLoggerConfig = HistoryLoggerConfig
  { syncLoggingFrequency :: Integer
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryLoggerConfig

historyLogger :: HistoryLoggerConfig -> Process ()
historyLogger HistoryLoggerConfig{..} = syncing 0 (0 :: Integer) (0 :: Integer)
  where
    syncing blocksSinceLastLog startedCount closedCount = do
      (inSync, startedCount', closedCount') <- receiveWait
        [ match $ pure . \case
          ContractWasCreated _        -> (False, startedCount + 1, closedCount)
          InputsWereApplied Nothing _ -> (False, startedCount, closedCount + 1)
          _                           -> (False, startedCount, closedCount)
        , match $ pure . \case
          ChainSyncStart point tip -> do
            let pointNo = getPointNo point
            let tipNo = getTipNo tip
            (pointNo == tipNo, startedCount, closedCount)
          ChainSyncEvent (MarloweRollBackward point tip) -> do
            let pointNo = getPointNo point
            let tipNo = getTipNo tip
            (pointNo == tipNo, startedCount, closedCount)
          ChainSyncEvent (MarloweRollForward (MarloweBlockHeader (MarloweSlotNo slot) _ _) _ tip) -> do
            let tipNo = getTipNo tip
            (slot == tipNo, startedCount, closedCount)
          ChainSyncDone -> (False, startedCount, closedCount)
        ]
      let blocksSinceLastLog' = (blocksSinceLastLog + 1) `mod` syncLoggingFrequency
      if inSync then do
        synced
      else if blocksSinceLastLog' == 0 then do
        when (closedCount' > 0 || startedCount' > 0) do
          say $ show startedCount' <> " contracts started, " <> show closedCount' <> " closed since last log"
        syncing 0 0 0
      else do
        syncing blocksSinceLastLog' startedCount' closedCount'
    synced = do
      receiveWait
        [ match \(_ :: ChainSyncMsg) -> pure ()
        , match \Event{..} ->
            case historyEvent of
              ContractWasCreated AppTxOutRef{..}              -> do
                say "New contract started"
                say $ "ContractId: " <> show contractId
                say $ "Datum: " <> show datum
                say $ "UTxO: " <> show txOutRef
              InputsWereApplied mTxOut inputs                -> do
                say "Inputs applied to contract"
                say $ "ContractId: " <> show contractId
                say $ "Inputs: " <> show inputs
                case mTxOut of
                  Nothing -> say "Contract was closed"
                  Just AppTxOutRef{..} -> do
                    say $ "New Datum: " <> show datum
                    say $ "New UTxO: " <> show txOutRef
              RoleWasPaidOut{..}                               -> do
                say "New payouts for role"
                say $ "ContractId: " <> show contractId
                say $ "Token name: " <> tokenName
                say $ "Assets: " <> show assets
                say $ "UTxO: " <> show payoutTxOut
        ]
      synced
    getTipNo MarloweChainTipAtGenesis                = 0
    getTipNo (MarloweChainTip (MarloweSlotNo n) _ _) = n
    getPointNo MarloweChainPointAtGenesis              = 0
    getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n

remotable ['historyLogger]

process :: HistoryLoggerConfig -> Closure (Process ())
process = $(mkClosure 'historyLogger)
