{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Proxy
  where

import Control.Concurrent.Component
import Control.Concurrent.Component.Probes
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Control.Monad.With (MonadWith(..))
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(..), GeneralReleaseType(..))
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server (MarloweRuntimeServer(..))
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Connection (SomeConnectionSource, SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.TypedProtocol (Driver)
import UnliftIO (MonadUnliftIO(..), catchSyncOrAsync, mask, throwIO)

newtype WrappedUnliftIO m a = WrappedUnliftIO { runWrappedUnliftIO :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch, MonadMask, MonadFail)

instance MonadUnliftIO m => MonadWith (WrappedUnliftIO m) where
  stateThreadingGeneralWith (GeneralAllocate allocA) go = mask $ \restore -> do
    GeneralAllocated a releaseA <- allocA restore
    b <- restore (go a) `catchSyncOrAsync` \e -> do
      _ <- releaseA $ ReleaseFailure e
      throwIO e
    c <- releaseA $ ReleaseSuccess b
    pure (b, c)

instance MonadIO m => MonadBase IO (WrappedUnliftIO m) where
  liftBase = liftIO

instance MonadUnliftIO m => MonadBaseControl IO (WrappedUnliftIO m) where
  type StM (WrappedUnliftIO m) a = a
  liftBaseWith = withRunInIO
  restoreM = pure

type ServerM = WrappedUnliftIO (ResourceT IO)

data ProxyDependencies = forall dState. ProxyDependencies
  { getMarloweSyncDriver :: ServerM (Driver (Handshake MarloweSync) dState ServerM)
  , getMarloweHeaderSyncDriver :: ServerM (Driver (Handshake MarloweHeaderSync) dState ServerM)
  , getMarloweQueryDriver :: ServerM (Driver (Handshake MarloweQuery) dState ServerM)
  , getTxJobDriver :: ServerM (Driver (Handshake (Job MarloweTxCommand)) dState ServerM)
  , connectionSource :: SomeConnectionSource MarloweRuntimeServer ServerM
  , httpPort :: Int
  }

proxy :: Component IO ProxyDependencies ()
proxy = proc deps -> do
  (serverComponent (component_ worker) \ProxyDependencies{..} -> do
    connector <- runResourceT $ runWrappedUnliftIO $ acceptSomeConnector connectionSource
    pure WorkerDependencies{..}) -< deps
  probeServer -< ProbeServerDependencies
    { probes = Probes
        { startup = pure True
        , liveness = pure True
        , readiness = pure True
        }
    , port = httpPort deps
    }

data WorkerDependencies = forall dState. WorkerDependencies
  { getMarloweSyncDriver :: ServerM (Driver (Handshake MarloweSync) dState ServerM)
  , getMarloweHeaderSyncDriver :: ServerM (Driver (Handshake MarloweHeaderSync) dState ServerM)
  , getMarloweQueryDriver :: ServerM (Driver (Handshake MarloweQuery) dState ServerM)
  , getTxJobDriver :: ServerM (Driver (Handshake (Job MarloweTxCommand)) dState ServerM)
  , connector :: SomeServerConnector MarloweRuntimeServer ServerM
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = runResourceT $ runWrappedUnliftIO $ runSomeConnector connector MarloweRuntimeServer{result = (), ..}
