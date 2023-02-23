{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Proxy
  where

import Control.Concurrent.Component
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, generalBracket)
import Control.Monad.Cleanup (MonadCleanup(..))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (ResourceT, runResourceT)
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server (MarloweServer(..))
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (SomeConnectionSource, SomeServerConnector, acceptSomeConnector, runSomeConnector)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.TypedProtocol (Driver)
import UnliftIO (MonadUnliftIO(..))

newtype WrappedUnliftIO m a = WrappedUnliftIO { runWrappedUnliftIO :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadThrow, MonadCatch, MonadMask, MonadFail)

instance MonadMask m => MonadCleanup (WrappedUnliftIO m) where
  generalCleanup = generalBracket

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
  , connectionSource :: SomeConnectionSource MarloweServer ServerM
  }

proxy :: Component IO ProxyDependencies ()
proxy = serverComponent (component_ worker) \ProxyDependencies{..} -> do
  connector <- runResourceT $ runWrappedUnliftIO $ acceptSomeConnector connectionSource
  pure WorkerDependencies{..}

data WorkerDependencies = forall dState. WorkerDependencies
  { getMarloweSyncDriver :: ServerM (Driver (Handshake MarloweSync) dState ServerM)
  , getMarloweHeaderSyncDriver :: ServerM (Driver (Handshake MarloweHeaderSync) dState ServerM)
  , getMarloweQueryDriver :: ServerM (Driver (Handshake MarloweQuery) dState ServerM)
  , getTxJobDriver :: ServerM (Driver (Handshake (Job MarloweTxCommand)) dState ServerM)
  , connector :: SomeServerConnector MarloweServer ServerM
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = runResourceT $ runWrappedUnliftIO $ runSomeConnector connector MarloweServer{result = (), ..}
