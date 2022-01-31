module Control.Logger.Effect.Console where

import Prelude

import Control.Logger (Logger(..)) as Logger
import Control.Logger.Effect (Logger)
import Control.Monad.Freer.Extras.Log (LogLevel(..), LogMessage(..))
import Effect.Class (liftEffect)
import Effect.Class.Console (error, log, warn)
import Effect.Console (info)

logger
  :: forall msg
   . (msg -> String)
  -> Logger msg
logger show = Logger.Logger $ liftEffect <<< case _ of
  LogMessage { _logLevel: Debug, _logMessageContent: msg } -> log (show msg)
  LogMessage { _logLevel: Info, _logMessageContent: msg } -> info (show msg)
  LogMessage { _logLevel: Notice, _logMessageContent: msg } -> info (show msg)
  LogMessage { _logLevel: Warning, _logMessageContent: msg } -> warn (show msg)
  LogMessage { _logLevel: Error, _logMessageContent: msg } -> error (show msg)
  LogMessage { _logLevel: Critical, _logMessageContent: msg } -> error
    (show msg)
  LogMessage { _logLevel: Alert, _logMessageContent: msg } -> error (show msg)
  LogMessage { _logLevel: Emergency, _logMessageContent: msg } -> error
    (show msg)
