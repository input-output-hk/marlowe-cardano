module Control.Logger.Capability where

-- | This is not strictly `Control.Logger.Logger` related but I've
-- | added it to the namespace so we have a single place
-- | for logging APIs implementations.

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Freer.Extras.Log (LogLevel(..), LogMessage(..))
import Control.Monad.Identity.Trans (IdentityT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)

class Monad m <= MonadLogger msg m where
  log :: LogMessage msg -> m Unit

instance MonadLogger msg m => MonadLogger msg (ContT r m) where
  log = lift <<< log

instance MonadLogger msg m => MonadLogger msg (IdentityT m) where
  log = lift <<< log

instance MonadLogger msg m => MonadLogger msg (ExceptT e' m) where
  log = lift <<< log

instance MonadLogger msg m => MonadLogger msg (MaybeT m) where
  log = lift <<< log

instance (Monoid w, MonadLogger msg m) => MonadLogger msg (RWST r w s m) where
  log = lift <<< log

instance MonadLogger msg m => MonadLogger msg (ReaderT r m) where
  log = lift <<< log

instance (Monoid w, MonadLogger msg m) => MonadLogger msg (WriterT w m) where
  log = lift <<< log

instance MonadLogger msg m => MonadLogger msg (StateT s m) where
  log = lift <<< log

debug :: forall m msg. MonadLogger msg m => msg -> m Unit
debug msg = log $ LogMessage { _logLevel: Debug, _logMessageContent: msg }

info :: forall m msg. MonadLogger msg m => msg -> m Unit
info msg = log $ LogMessage { _logLevel: Debug, _logMessageContent: msg }

warning :: forall m msg. MonadLogger msg m => msg -> m Unit
warning msg = log $ LogMessage { _logLevel: Debug, _logMessageContent: msg }

error :: forall m msg. MonadLogger msg m => msg -> m Unit
error msg = log $ LogMessage { _logLevel: Debug, _logMessageContent: msg }
