module Control.Logger.Effect.Class where

-- | Logging which relies on `MonadEffect`.

import Prelude

import Control.Logger (log) as Control.Logger
import Control.Logger.Effect (Logger)
import Control.Monad.Freer.Extras.Log (LogLevel(..), LogMessage(..))
import Control.Monad.Reader (class MonadReader, asks)
import Effect.Class (class MonadEffect, liftEffect)

log :: forall m msg. MonadEffect m => Logger msg -> LogMessage msg -> m Unit
log logger logMsg = liftEffect (Control.Logger.log logger logMsg)

debug :: forall m msg. MonadEffect m => msg -> Logger msg -> m Unit
debug msg l = log l $ LogMessage { _logLevel: Debug, _logMessageContent: msg }

info :: forall m msg. MonadEffect m => msg -> Logger msg -> m Unit
info msg l = log l $ LogMessage { _logLevel: Info, _logMessageContent: msg }

warning :: forall m msg. MonadEffect m => msg -> Logger msg -> m Unit
warning msg l = log l $ LogMessage
  { _logLevel: Warning, _logMessageContent: msg }

error :: forall m msg. MonadEffect m => msg -> Logger msg -> m Unit
error msg l = log l $ LogMessage { _logLevel: Error, _logMessageContent: msg }

log'
  :: forall ctx m msg
   . MonadEffect m
  => MonadReader { logger :: Logger msg | ctx } m
  => LogMessage msg
  -> m Unit
log' logMsg = do
  logger <- asks _.logger
  liftEffect $ log logger logMsg

debug'
  :: forall ctx m msg
   . MonadEffect m
  => MonadReader { logger :: Logger msg | ctx } m
  => msg
  -> m Unit
debug' msg = asks _.logger >>= debug msg >>> liftEffect

info'
  :: forall ctx m msg
   . MonadEffect m
  => MonadReader { logger :: Logger msg | ctx } m
  => msg
  -> m Unit
info' msg = asks _.logger >>= info msg >>> liftEffect

warning'
  :: forall ctx m msg
   . MonadEffect m
  => MonadReader { logger :: Logger msg | ctx } m
  => msg
  -> m Unit
warning' msg = asks _.logger >>= warning msg >>> liftEffect

error'
  :: forall ctx m msg
   . MonadEffect m
  => MonadReader { logger :: Logger msg | ctx } m
  => msg
  -> m Unit
error' msg = asks _.logger >>= error msg >>> liftEffect

