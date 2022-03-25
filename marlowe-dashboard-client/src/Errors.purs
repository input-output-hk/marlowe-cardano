module Errors
  ( globalError
  ) where

import Prologue

import Capability.Toast (class Toast, addToast)
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Structured (StructuredLog)
import Control.Logger.Structured as Logger
import Errors.Debuggable (class Debuggable)
import Errors.Explain (class Explain)
import Toast.Types (explainableErrorToast)

globalError
  :: forall m error
   . Toast m
  => MonadLogger StructuredLog m
  => Explain error
  => Debuggable error
  => String
  -> error
  -> m Unit
globalError shortDescription error = do
  addToast $ explainableErrorToast shortDescription error
  Logger.error shortDescription error

