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
import Control.Lens (zoom)
import Control.Monad.Except (runExceptT, throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader(ask), ReaderT(runReaderT))
import Control.Monad.State (MonadState(get, put), StateT(runStateT), evalStateT, execStateT)
import Language.Marlowe.CLI.Cardano.Api.PlutusScript (IsPlutusScriptLanguage)
import qualified Language.Marlowe.CLI.Test.CLI.Interpret as CLI
import qualified Language.Marlowe.CLI.Test.Runtime.Interpret as Runtime
import Language.Marlowe.CLI.Test.Types
  ( InterpretMonad
  , TestOperation(CLIOperation, Fail, RuntimeOperation, WalletOperation)
  , cliInpterpretStateL
  , runtimeInpterpretStateL
  , toCLIInterpretEnv
  , toRuntimeInterpretEnv
  , toWalletInterpretEnv
  , walletInpterpretStateL
  )
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
  state <- get
  env <- ask
  case toRuntimeInterpretEnv env of
    Nothing -> throwError $ CliError "Runtime env not initialized. Aborting."
    Just env' -> do
      let
        interpret' = zoom runtimeInpterpretStateL (Runtime.interpret ro)
      e <- liftIO $ runExceptT (runReaderT (execStateT interpret' state) env')
      either throwError put e
interpret (WalletOperation wo) = do
  state <- get
  env <- ask
  let
    interpret' = zoom walletInpterpretStateL (Wallet.interpret wo)
    env' = toWalletInterpretEnv env
  e <- liftIO $ runExceptT (runReaderT (execStateT interpret' state) env')
  either throwError put e
interpret (CLIOperation wo) = do
  state <- get
  env <- ask
  let
    interpret' = zoom cliInpterpretStateL (CLI.interpret wo)
    env' = toCLIInterpretEnv env
  e <- liftIO $ runExceptT (runReaderT (execStateT interpret' state) env')
  either throwError put e
interpret (Fail message) =
  throwError $ CliError message

