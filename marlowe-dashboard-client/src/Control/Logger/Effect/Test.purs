module Control.Logger.Effect.Test where

import Prologue

import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Logger (Logger(..)) as Logger
import Control.Logger.Effect (Logger)
import Control.Monad.Freer.Extras.Log (LogMessage)
import Effect.Aff (launchAff_)

testLogger :: forall a. Queue (LogMessage a) -> Logger a
testLogger queue = Logger.Logger $ launchAff_ <<< Queue.write queue
