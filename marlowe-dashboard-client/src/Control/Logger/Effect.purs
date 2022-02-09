module Control.Logger.Effect where

-- | We reuse existing generated types for logging from Haskell `freer-extra`.
-- | This package provides a large spectrum of logging levels (I think isomorphic
-- | to journald levels) but I narrowed all helpers down
-- | so the expose only four log levels: Debug, Info, Warning, Error.

import Prelude

import Control.Logger (Logger, cfilter) as Logger
import Control.Monad.Freer.Extras.Log (LogLevel, LogMessage(..))
import Effect (Effect)

type Logger msg = Logger.Logger Effect (LogMessage msg)

filterLogLevel
  :: forall m msg
   . Applicative m
  => LogLevel
  -> Logger (LogMessage msg)
  -> Logger (LogMessage msg)
filterLogLevel minLogLevel = Logger.cfilter
  (\(LogMessage r) -> r._logLevel >= minLogLevel)

