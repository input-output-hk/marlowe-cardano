module Test.Control.Monad.Time where

import Prelude

import Data.Time.Duration (class Duration)

class Monad m <= MonadMockTime m where
  advanceTime :: forall d. Duration d => d -> m Unit
