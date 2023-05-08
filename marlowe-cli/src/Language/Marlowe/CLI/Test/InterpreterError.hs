{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.CLI.Test.InterpreterError
  where

import Control.Error (note)
import Control.Monad.Except (ExceptT, MonadError(throwError), liftEither, runExceptT)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as A.Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.IO (liftCli)
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operations
import Language.Marlowe.CLI.Types (CliError(CliError))

data InterpreterError
  = CliOperationFailed
    { ieMessage :: String
    , ieInfo :: [(A.Key, A.Value)]
    }
  | RuntimeOperationFailed
    { ieMessage :: String
    , ieInfo :: [(A.Key, A.Value)]
    }
  | TimeOutReached
    { ieMessage :: String
    , ieInfo :: [(A.Key, A.Value)]
    }
  | TestExecutionFailed
    { ieMessage :: String
    , ieInfo :: [(A.Key, A.Value)]
    }
  -- | InvalidTestCase
  --   { ieMessage :: String
  --   , ieInfo :: [(A.Key, A.Value)]
  --   }
  | AssertionFailed
    { ieMessage :: String
    , ieInfo :: [(A.Key, A.Value)]
    }
  deriving stock (Show, Eq, Generic)

instance A.FromJSON InterpreterError where
  parseJSON = Operations.parseConstructorBasedJSON "ie"

instance A.ToJSON InterpreterError where
  toJSON = Operations.toConstructorBasedJSON "ie"

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
  :: MonadError InterpreterError m
  => ExceptT CliError m a
  -> m a
rethrowCliError action =
  runExceptT action >>= \case
    Left err -> throwError $ fromCliError' err
    Right contract -> pure contract

