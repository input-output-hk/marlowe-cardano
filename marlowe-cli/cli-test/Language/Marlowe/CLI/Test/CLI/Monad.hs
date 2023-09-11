{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.CLI.Test.CLI.Monad where

import Cardano.Api (ScriptDataSupportedInEra)
import Control.Monad.Except (ExceptT, MonadError (catchError, throwError), runExceptT)
import Control.Monad.Reader (ReaderT (runReaderT))
import Data.Aeson qualified as A
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError, fromCliError)
import Language.Marlowe.CLI.Test.Log (Label (label))
import Language.Marlowe.CLI.Types (CliEnv (CliEnv), CliError (CliError))

-- This helper is present in the newer version of mtl
withError :: (MonadError e m) => (e -> e) -> m a -> m a
withError modifyError action =
  catchError action \e -> throwError $ modifyError e

withCliErrorMsg :: (MonadError CliError m) => (String -> String) -> m a -> m a
withCliErrorMsg f = withError (\(CliError msg) -> CliError (f msg))

-- `modifyError` was added to the newer version of mtl
-- So we have to do some gimnastics here.
runCli
  :: (MonadError InterpreterError m)
  => ScriptDataSupportedInEra era
  -> String
  -> ReaderT (CliEnv era) (ExceptT CliError m) a
  -> m a
runCli era msg action = do
  res <- runExceptT (runReaderT action (CliEnv era))
  case res of
    Left err -> do
      let info = [("label", A.toJSON msg)]
      throwError $ fromCliError err info
    Right a -> pure a

runLabeledCli
  :: (MonadError InterpreterError m)
  => (Label l)
  => ScriptDataSupportedInEra era
  -> l
  -> ReaderT (CliEnv era) (ExceptT CliError m) a
  -> m a
runLabeledCli era l = runCli era (label l)
