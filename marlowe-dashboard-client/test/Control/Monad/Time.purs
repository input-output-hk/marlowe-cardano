module Test.Control.Monad.Time where

import Prelude

import Control.Monad.Cont (ContT)
import Control.Monad.Except (ExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.RWS (RWST)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Data.Time.Duration (class Duration)

class Monad m <= MonadMockTime m where
  advanceTime :: forall d. Duration d => d -> m Unit

instance MonadMockTime m => MonadMockTime (ReaderT r m) where
  advanceTime = map lift advanceTime

instance (Monoid w, MonadMockTime m) => MonadMockTime (WriterT w m) where
  advanceTime = map lift advanceTime

instance MonadMockTime m => MonadMockTime (StateT s m) where
  advanceTime = map lift advanceTime

instance MonadMockTime m => MonadMockTime (ContT r m) where
  advanceTime = map lift advanceTime

instance MonadMockTime m => MonadMockTime (ExceptT e m) where
  advanceTime = map lift advanceTime

instance MonadMockTime m => MonadMockTime (MaybeT m) where
  advanceTime = map lift advanceTime

instance (Monoid w, MonadMockTime m) => MonadMockTime (RWST r w s m) where
  advanceTime = map lift advanceTime
