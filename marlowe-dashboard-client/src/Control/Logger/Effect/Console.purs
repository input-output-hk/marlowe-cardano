module Control.Logger.Effect.Console where

import Prologue

import Control.Logger (Logger(..)) as Logger
import Control.Logger.Effect (Logger)
import Control.Logger.Structured (StructuredLog(..))
import Control.Monad.Freer.Extras.Log (LogLevel(..), LogMessage(..))
import Data.Argonaut (Json)
import Effect (Effect)
import Effect.Console (error, info, log, warn)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)

foreign import debugImpl :: EffectFn1 String Unit

debug :: String -> Effect Unit
debug = runEffectFn1 debugImpl

stringLogger
  :: forall msg
   . (msg -> String)
  -> Logger msg
stringLogger show = Logger.Logger case _ of
  LogMessage { _logLevel: Debug, _logMessageContent: msg } -> debug (show msg)
  LogMessage { _logLevel: Info, _logMessageContent: msg } -> info (show msg)
  LogMessage { _logLevel: Notice, _logMessageContent: msg } -> log (show msg)
  LogMessage { _logLevel: Warning, _logMessageContent: msg } -> warn (show msg)
  LogMessage { _logLevel: Error, _logMessageContent: msg } -> error (show msg)
  LogMessage { _logLevel: Critical, _logMessageContent: msg } -> error
    (show msg)
  LogMessage { _logLevel: Alert, _logMessageContent: msg } -> error (show msg)
  LogMessage { _logLevel: Emergency, _logMessageContent: msg } -> error
    (show msg)

foreign import structuredDebugImpl :: EffectFn2 String Json Unit

structuredDebug :: String -> Json -> Effect Unit
structuredDebug = runEffectFn2 structuredDebugImpl

foreign import structuredInfoImpl :: EffectFn2 String Json Unit

structuredInfo :: String -> Json -> Effect Unit
structuredInfo = runEffectFn2 structuredInfoImpl

foreign import structuredLogImpl :: EffectFn2 String Json Unit

structuredLog :: String -> Json -> Effect Unit
structuredLog = runEffectFn2 structuredLogImpl

foreign import structuredWarnImpl :: EffectFn2 String Json Unit

structuredWarn :: String -> Json -> Effect Unit
structuredWarn = runEffectFn2 structuredWarnImpl

foreign import structuredErrorImpl :: EffectFn2 String Json Unit

structuredError :: String -> Json -> Effect Unit
structuredError = runEffectFn2 structuredErrorImpl

structuredLogger :: Logger StructuredLog
structuredLogger = Logger.Logger case _ of
  LogMessage
    { _logLevel: Debug
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> debug msg
  LogMessage
    { _logLevel: Debug
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredDebug msg p
  LogMessage
    { _logLevel: Info
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> info msg
  LogMessage
    { _logLevel: Info
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredInfo msg p
  LogMessage
    { _logLevel: Notice
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> log msg
  LogMessage
    { _logLevel: Notice
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredLog msg p
  LogMessage
    { _logLevel: Warning
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> warn msg
  LogMessage
    { _logLevel: Warning
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredWarn msg p
  LogMessage
    { _logLevel: Error
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> error msg
  LogMessage
    { _logLevel: Error
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredError msg p
  LogMessage
    { _logLevel: Critical
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> error msg
  LogMessage
    { _logLevel: Critical
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredError msg p
  LogMessage
    { _logLevel: Alert
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> error msg
  LogMessage
    { _logLevel: Alert
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredError msg p
  LogMessage
    { _logLevel: Emergency
    , _logMessageContent: StructuredLog { msg, payload: Nothing }
    } -> error msg
  LogMessage
    { _logLevel: Emergency
    , _logMessageContent: StructuredLog { msg, payload: Just p }
    } -> structuredError msg p
