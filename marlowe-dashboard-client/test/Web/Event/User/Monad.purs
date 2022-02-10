module Test.Web.Event.User.Monad where

import Prelude

import Control.Monad.Cont (ContT, mapContT)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Maybe.Trans (MaybeT, mapMaybeT)
import Control.Monad.RWS (RWST, mapRWST)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, mapWriterT)
import Test.Web (TestM, hoistTestM)
import Test.Web.Event.User.Api (UserApi)
import Test.Web.Event.User.Options (UserOptions)

class Monad m <= MonadUser m where
  api :: m UserApi
  setup :: forall a. UserOptions -> m a -> m a

instance MonadUser m => MonadUser (TestM m) where
  api = lift api
  setup options = hoistTestM (setup options)

instance MonadUser m => MonadUser (ReaderT r m) where
  api = lift api
  setup options = mapReaderT $ setup options

instance (Monoid w, MonadUser m) => MonadUser (WriterT w m) where
  api = lift api
  setup options = mapWriterT $ setup options

instance MonadUser m => MonadUser (StateT s m) where
  api = lift api
  setup options = mapStateT $ setup options

instance MonadUser m => MonadUser (ContT r m) where
  api = lift api
  setup options = mapContT $ setup options

instance MonadUser m => MonadUser (ExceptT e m) where
  api = lift api
  setup options = mapExceptT $ setup options

instance MonadUser m => MonadUser (MaybeT m) where
  api = lift api
  setup options = mapMaybeT $ setup options

instance (Monoid w, MonadUser m) => MonadUser (RWST r w s m) where
  api = lift api
  setup options = mapRWST $ setup options
