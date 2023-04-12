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

module Language.Marlowe.CLI.Test.CLI.Monad
  where

import Cardano.Api (ScriptDataSupportedInEra)
import Control.Monad.Except (MonadError(catchError, throwError))
import Control.Monad.Reader (ReaderT(runReaderT))
import Language.Marlowe.CLI.Test.Log (Label, LabelFormat(LabelConstructorName), printLabeledMsg, printTraceMsg)
import Language.Marlowe.CLI.Types (CliEnv(CliEnv), CliError(CliError))
import Ledger.Orphans ()

-- This helper is present in the newer version of mtl
withError :: MonadError e m => (e -> e) -> m a -> m a
withError modifyError action =
  catchError action \e -> throwError $ modifyError e

withCliErrorMsg :: MonadError CliError m => (String -> String) -> m a -> m a
withCliErrorMsg f = withError (\(CliError msg) -> CliError (f msg))

runCli
  :: MonadError CliError m
  => ScriptDataSupportedInEra era
  -> String
  -> ReaderT (CliEnv era) m a
  -> m a
runCli era msg action = do
  withCliErrorMsg (mappend msg) $ runReaderT action (CliEnv era)

runTraceCli
  :: MonadError CliError m
  => ScriptDataSupportedInEra era
  -> String
  -> ReaderT (CliEnv era) m a
  -> m a
runTraceCli era trace action = do
  withCliErrorMsg (printTraceMsg trace) $ runReaderT action (CliEnv era)

runLabeledCli
  :: MonadError CliError m
  => Label l
  => ScriptDataSupportedInEra era
  -> l
  -> ReaderT (CliEnv era) m a
  -> m a
runLabeledCli era l action = do
  withCliErrorMsg (printLabeledMsg LabelConstructorName l) $ runReaderT action (CliEnv era)

