module Control.Logger.Effect.Test where

import Prologue

import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Logger (Logger(..)) as Logger
import Control.Logger.Effect (Logger)
import Control.Logger.Structured (StructuredLog)
import Control.Monad.Freer.Extras.Log (LogMessage)
import Effect.Aff (launchAff_)

testLogger :: Queue (LogMessage StructuredLog) -> Logger StructuredLog
testLogger queue = Logger.Logger $ launchAff_ <<< Queue.write queue
