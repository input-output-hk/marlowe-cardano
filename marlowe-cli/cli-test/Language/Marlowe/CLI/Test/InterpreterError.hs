{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.CLI.Test.InterpreterError where

import Control.Lens (makeLenses)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Data.Aeson qualified as A
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operations
import Language.Marlowe.CLI.Types (CliError (CliError))

data InterpreterError
  = CliOperationFailed
      { _ieMessage :: String
      , _ieInfo :: [(A.Key, A.Value)]
      }
  | SimulationOperationFailed
      { _ieMessage :: String
      , _ieInfo :: [(A.Key, A.Value)]
      }
  | RuntimeOperationFailed
      { _ieMessage :: String
      , _ieInfo :: [(A.Key, A.Value)]
      }
  | TimeOutReached
      { _ieMessage :: String
      , _ieInfo :: [(A.Key, A.Value)]
      }
  | TestExecutionFailed
      { _ieMessage :: String
      , _ieInfo :: [(A.Key, A.Value)]
      }
  | -- | InvalidTestCase
    --   { ieMessage :: String
    --   , ieInfo :: [(A.Key, A.Value)]
    --   }
    AssertionFailed
      { _ieMessage :: String
      , _ieInfo :: [(A.Key, A.Value)]
      }
  deriving stock (Show, Eq, Generic)

makeLenses ''InterpreterError

instance A.FromJSON InterpreterError where
  parseJSON = Operations.parseConstructorBasedJSON' "_ie"

instance A.ToJSON InterpreterError where
  toJSON = Operations.toConstructorBasedJSON "_ie"

fromCliError :: CliError -> [(A.Key, A.Value)] -> InterpreterError
fromCliError (CliError msg) = CliOperationFailed msg

fromCliError' :: CliError -> InterpreterError
fromCliError' (CliError msg) = CliOperationFailed msg []

testExecutionFailed' :: String -> InterpreterError
testExecutionFailed' msg = TestExecutionFailed msg []

assertionFailed' :: String -> InterpreterError
assertionFailed' msg = AssertionFailed msg []

timeoutReached' :: String -> InterpreterError
timeoutReached' msg = TimeOutReached msg []

runtimeOperationFailed' :: String -> InterpreterError
runtimeOperationFailed' msg = RuntimeOperationFailed msg []

rethrowCliError
  :: (MonadError InterpreterError m)
  => ExceptT CliError m a
  -> m a
rethrowCliError action =
  runExceptT action >>= \case
    Left err -> throwError $ fromCliError' err
    Right contract -> pure contract
