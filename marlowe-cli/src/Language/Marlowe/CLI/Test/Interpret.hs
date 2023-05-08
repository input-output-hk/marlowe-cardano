{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module Language.Marlowe.CLI.Test.Interpret
  where

import Cardano.Api (IsShelleyBasedEra)
import Contrib.Control.Concurrent (threadDelay)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import qualified Language.Marlowe.CLI.Test.CLI.Interpret as CLI
import Language.Marlowe.CLI.Test.InterpreterError (testExecutionFailed')
import Language.Marlowe.CLI.Test.Log (logLabeledMsg)
import qualified Language.Marlowe.CLI.Test.Runtime.Interpret as Runtime
import Language.Marlowe.CLI.Test.Types
  (InterpretMonad, TestOperation(CLIOperation, Fail, RuntimeOperation, Sleep, WalletOperation))
import qualified Language.Marlowe.CLI.Test.Wallet.Interpret as Wallet
import Language.Marlowe.CLI.Types (CliError(CliError))

interpret
  :: forall m lang era
   . IsShelleyBasedEra era
  => IsPlutusScriptLanguage lang
  => InterpretMonad m lang era
  => TestOperation
  -> m ()
interpret (RuntimeOperation ro) = do
  logLabeledMsg ro ""
  Runtime.interpret ro
interpret (WalletOperation wo) = do
  logLabeledMsg wo ""
  Wallet.interpret wo
interpret (CLIOperation co) = do
  logLabeledMsg co ""
  CLI.interpret co
interpret o@(Sleep seconds) = do
  logLabeledMsg o ""
  logLabeledMsg o $ "Sleeping for " <> show seconds
  liftIO $ threadDelay seconds
interpret (Fail message) =
  throwError $ testExecutionFailed' message

