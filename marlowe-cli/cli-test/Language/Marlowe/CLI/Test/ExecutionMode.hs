{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.CLI.Test.ExecutionMode
  where


import Control.Monad.IO.Class (MonadIO)
import Data.Time.Units (Second)
import Language.Marlowe.CLI.Test.Log (Label, logLabeledMsg)
import Language.Marlowe.CLI.Types (SubmitMode(DoSubmit, DontSubmit))

-- | Configuration for executing Marlowe CLI DSL commands on the blockchain
-- | The idea behind simulation mode is to use it as a "first line of defense" to detect
-- | errors in code or in the test case itself before spending the time/resources to run the same
-- | scenarios on chain.
-- | Tests that fail in simulation mode should also fail on chain.
-- | Tests that pass on chain should also pass in the simulation mode.
-- | Configuration for executing Marlowe CLI DSL commands on the blockchain.
data ExecutionMode =
    SimulationMode
  | OnChainMode { transactionSubmissionTimeout :: Second }
    deriving stock (Eq, Show)

toSubmitMode :: ExecutionMode -> SubmitMode
toSubmitMode SimulationMode = DontSubmit
toSubmitMode (OnChainMode timeout) = DoSubmit timeout

skipInSimluationMode
  :: forall l m
   . Label l
  => MonadIO m
  => l
  -> m ()
  -> ExecutionMode
  -> m ()
skipInSimluationMode label action = \case
  SimulationMode ->
    logLabeledMsg label "Skipping in simulation mode."
  OnChainMode {} -> action

