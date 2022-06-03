{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
module History.Logger where

import ChainSync.Client (ChainSyncMsg (..))
import Control.Distributed.Process (Closure, Process, match, receiveWait, say)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Data.Binary (Binary)
import Data.Data (Typeable)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainEvent (..), MarloweChainPoint (..),
                                             MarloweChainTip (..), MarloweSlotNo (..), MarloweTxOut (..))
import Language.Marlowe.Runtime.History.Types (AppTxOutRef (..), ContractCreationTxOut (..), Event (..),
                                               HistoryEvent (..))

newtype HistoryLoggerConfig = HistoryLoggerConfig
  { syncLoggingFrequency :: Integer
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryLoggerConfig

historyLogger :: HistoryLoggerConfig -> Process ()
historyLogger _ = syncing
  where
    syncing = do
      inSync <- receiveWait
        [ match $ \(_ :: Event) -> pure False
        , match $ pure . \case
          ChainSyncStart point tip -> do
            let pointNo = getPointNo point
            let tipNo = getTipNo tip
            pointNo == tipNo
          ChainSyncEvent (MarloweRollBackward point tip) -> do
            let pointNo = getPointNo point
            let tipNo = getTipNo tip
            pointNo == tipNo
          ChainSyncEvent (MarloweRollForward (MarloweBlockHeader (MarloweSlotNo slot) _ _) _ tip) -> do
            let tipNo = getTipNo tip
            slot == tipNo
          ChainSyncDone -> False
        ]
      if inSync then synced else syncing
    synced = do
      receiveWait
        [ match \(_ :: ChainSyncMsg) -> pure ()
        , match \Event{..} ->
            case historyEvent of
              ContractWasCreated ContractCreationTxOut{txOut, datum}              -> do
                say "New contract started"
                say $ "  ContractId: " <> show contractId
                say $ "  Datum: " <> show datum
                say $ "  UTxO: " <> show (marloweTxOut_txOutRef txOut)
              InputsWereApplied mTxOut inputs                -> do
                say "Inputs applied to contract"
                say $ "  ContractId: " <> show contractId
                say $ "  Inputs: " <> show inputs
                case mTxOut of
                  Nothing -> say "Contract was closed"
                  Just AppTxOutRef{..} -> do
                    say $ "  New Datum: " <> show datum
                    say $ "  New UTxO: " <> show txOutRef
              RoleWasPaidOut{..}                               -> do
                say "New payouts for role"
                say $ "  ContractId: " <> show contractId
                say $ "  Token name: " <> tokenName
                say $ "  Assets: " <> show assets
                say $ "  UTxO: " <> show payoutTxOut
              PayoutWasRedeemed txOutRef                               -> do
                say "Payout was redeemed"
                say $ "  ContractId: " <> show contractId
                say $ "  TxIn: " <> show txOutRef
        ]
      synced
    getTipNo MarloweChainTipAtGenesis                = 0
    getTipNo (MarloweChainTip (MarloweSlotNo n) _ _) = n
    getPointNo MarloweChainPointAtGenesis              = 0
    getPointNo (MarloweChainPoint (MarloweSlotNo n) _) = n

remotable ['historyLogger]

process :: HistoryLoggerConfig -> Closure (Process ())
process = $(mkClosure 'historyLogger)
