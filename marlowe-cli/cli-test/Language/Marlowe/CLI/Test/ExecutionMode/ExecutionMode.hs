{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.CLI.Test.ExecutionMode.ExecutionMode where

import Cardano.Api qualified as C
import Control.Concurrent.STM (TVar)
import Control.Concurrent.STM.TVar (newTVarIO)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Time.Units (Second)
import Language.Marlowe.CLI.Test.Log (Label, logLabeledMsg)
import Language.Marlowe.CLI.Types (NodeStateInfo, SubmitMode (DoSubmit, DontSubmit))

data UseExecutionMode
  = UseSimulationMode
  | UseOnChainMode Second

-- | Configuration for executing Marlowe CLI DSL commands on the blockchain
-- | The idea behind simulation mode is to use it as a "first line of defense" to detect
-- | errors in code or in the test case itself before spending the time/resources to run the same
-- | scenarios on the chain.
-- | Tests that fail in simulation mode should also fail on chain.
-- | Tests that pass on chain should also pass in the simulation mode.
-- | Configuration for executing Marlowe CLI DSL commands on the blockchain.
data ExecutionMode era
  = SimulationMode (TVar (C.UTxO era)) (NodeStateInfo era)
  | OnChainMode {transactionSubmissionTimeout :: Second}

newSimulationMode
  :: forall era m
   . (MonadIO m)
  => C.UTxO era
  -> NodeStateInfo era
  -> m (ExecutionMode era)
newSimulationMode utxo nsi = liftIO do
  tvar <- newTVarIO utxo
  pure $ SimulationMode tvar nsi

toSubmitMode :: forall era. ExecutionMode era -> SubmitMode
toSubmitMode (SimulationMode _ _) = DontSubmit
toSubmitMode (OnChainMode timeout) = DoSubmit timeout

skipInSimluationMode
  :: forall era l m
   . (Label l)
  => (MonadIO m)
  => l
  -> m ()
  -> ExecutionMode era
  -> m ()
skipInSimluationMode label action = \case
  SimulationMode _ _ ->
    logLabeledMsg label "Skipping in simulation mode."
  OnChainMode{} -> action
