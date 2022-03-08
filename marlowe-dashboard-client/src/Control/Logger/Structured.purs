module Control.Logger.Structured where

import Prologue

import Control.Logger.Capability (class MonadLogger, log)
import Control.Monad.Freer.Extras.Log (LogLevel(..), LogMessage(..))
import Data.Argonaut (Json)
import Errors (class Debuggable, debuggable)

newtype StructuredLog = StructuredLog
  { msg :: String
  , payload :: Maybe Json
  }

debug
  :: forall m payload
   . MonadLogger StructuredLog m
  => Debuggable payload
  => String
  -> payload
  -> m Unit
debug msg payload = log $ LogMessage
  { _logLevel: Debug
  , _logMessageContent: StructuredLog
      { msg, payload: (Just $ debuggable payload) }
  }

debug' :: forall m. MonadLogger StructuredLog m => String -> m Unit
debug' msg = log $ LogMessage
  { _logLevel: Debug
  , _logMessageContent: StructuredLog { msg, payload: Nothing }
  }

info
  :: forall m payload
   . MonadLogger StructuredLog m
  => Debuggable payload
  => String
  -> payload
  -> m Unit
info msg payload = log $ LogMessage
  { _logLevel: Info
  , _logMessageContent: StructuredLog
      { msg, payload: (Just $ debuggable payload) }
  }

info' :: forall m. MonadLogger StructuredLog m => String -> m Unit
info' msg = log $ LogMessage
  { _logLevel: Info
  , _logMessageContent: StructuredLog { msg, payload: Nothing }
  }

warning
  :: forall m payload
   . MonadLogger StructuredLog m
  => Debuggable payload
  => String
  -> payload
  -> m Unit
warning msg payload = log $ LogMessage
  { _logLevel: Warning
  , _logMessageContent: StructuredLog
      { msg, payload: (Just $ debuggable payload) }
  }

warning' :: forall m. MonadLogger StructuredLog m => String -> m Unit
warning' msg = log $ LogMessage
  { _logLevel: Warning
  , _logMessageContent: StructuredLog { msg, payload: Nothing }
  }

error
  :: forall m payload
   . MonadLogger StructuredLog m
  => Debuggable payload
  => String
  -> payload
  -> m Unit
error msg payload = log $ LogMessage
  { _logLevel: Error
  , _logMessageContent: StructuredLog
      { msg, payload: (Just $ debuggable payload) }
  }

error' :: forall m. MonadLogger StructuredLog m => String -> m Unit
error' msg = log $ LogMessage
  { _logLevel: Error
  , _logMessageContent: StructuredLog { msg, payload: Nothing }
  }
