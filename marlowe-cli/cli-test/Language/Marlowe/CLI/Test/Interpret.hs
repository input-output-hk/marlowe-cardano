{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.CLI.Test.Interpret where

import Cardano.Api (IsShelleyBasedEra)
import Cardano.Api qualified as C
import Contrib.Control.Concurrent (threadDelay)
import Control.Monad.Except (MonadError (throwError), catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (get)
import Data.Aeson qualified as A
import Data.Aeson.Text qualified as A
import Data.Text.Lazy qualified as TL
import Language.Marlowe.CLI.Test.CLI.Interpret qualified as CLI
import Language.Marlowe.CLI.Test.InterpreterError (testExecutionFailed')
import Language.Marlowe.CLI.Test.Log (Label (label), logLabeledMsg, logStoreLabeledMsg, throwLabeledError)
import Language.Marlowe.CLI.Test.Runtime.Interpret qualified as Runtime
import Language.Marlowe.CLI.Test.Types (
  InterpretMonad,
  TestOperation (CLIOperation, Comment, Fail, RuntimeOperation, ShouldFail, Sleep, WalletOperation),
  interpretStateToJSONPairs,
 )
import Language.Marlowe.CLI.Test.Wallet.Interpret qualified as Wallet

interpret
  :: forall m era
   . (IsShelleyBasedEra era)
  => (InterpretMonad m C.PlutusScriptV2 era)
  => TestOperation
  -> m ()
interpret (RuntimeOperation ro) = do
  logStoreLabeledMsg ro ""
  Runtime.interpret ro
interpret (WalletOperation wo) = do
  logStoreLabeledMsg wo ""
  Wallet.interpret wo
interpret (CLIOperation co) = do
  logStoreLabeledMsg co ""
  CLI.interpret co
interpret o@(Sleep seconds) = do
  logStoreLabeledMsg o ""
  logStoreLabeledMsg o $ "Sleeping for " <> show seconds
  liftIO $ threadDelay seconds
interpret (Fail message) =
  throwError $ testExecutionFailed' message
interpret o@(Comment msg) = logLabeledMsg o msg
interpret o@(ShouldFail operation) =
  do
    logStoreLabeledMsg o $ label operation
    state <- get
    result <- (Right <$> interpret operation) `catchError` (pure . Left)
    case result of
      Right _ ->
        throwLabeledError o $ testExecutionFailed' $ "Operation unexpectedly succeeded: " <> show operation
      Left e -> do
        state' <- get
        prevStateJson <- A.object <$> interpretStateToJSONPairs state
        newStateJson <- A.object <$> interpretStateToJSONPairs state'
        -- FIXME: We don't have Eq instance in place and this check covers only the part of the state.
        if prevStateJson /= newStateJson
          then do
            prevStateStr <- TL.unpack . A.encodeToLazyText . A.object <$> interpretStateToJSONPairs state
            newStateStr <- TL.unpack . A.encodeToLazyText . A.object <$> interpretStateToJSONPairs state'
            throwLabeledError o $
              testExecutionFailed' $
                "Operation failed as expected: "
                  <> show e
                  <> " occurred for "
                  <> show operation
                  <> ", but it was able to change the state which is not allowed."
                  <> " State before: "
                  <> prevStateStr
                  <> "."
                  <> " State after: "
                  <> newStateStr
                  <> "."
          else logLabeledMsg o $ "Operation failed as expected: " <> show e <> " occurred for " <> show operation <> "."
