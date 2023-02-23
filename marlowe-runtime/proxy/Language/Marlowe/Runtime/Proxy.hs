{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Language.Marlowe.Runtime.Proxy
  where

import Control.Concurrent.Component
import Language.Marlowe.Protocol.HeaderSync.Types (MarloweHeaderSync)
import Language.Marlowe.Protocol.Query.Types (MarloweQuery)
import Language.Marlowe.Protocol.Server (MarloweServer(..))
import Language.Marlowe.Protocol.Sync.Types (MarloweSync)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (SomeConnectionSource, SomeServerConnector, acceptSomeConnector, runSomeConnector)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Job.Types (Job)
import Network.TypedProtocol (Driver)

data ProxyDependencies = forall dState. ProxyDependencies
  { getMarloweSyncDriver :: IO (Driver (Handshake MarloweSync) dState IO)
  , getMarloweHeaderSyncDriver :: IO (Driver (Handshake MarloweHeaderSync) dState IO)
  , getMarloweQueryDriver :: IO (Driver (Handshake MarloweQuery) dState IO)
  , getTxJobDriver :: IO (Driver (Handshake (Job MarloweTxCommand)) dState IO)
  , connectionSource :: SomeConnectionSource MarloweServer IO
  }

proxy :: Component IO ProxyDependencies ()
proxy = serverComponent (component_ worker) \ProxyDependencies{..} -> do
  connector <- acceptSomeConnector connectionSource
  pure WorkerDependencies{..}

data WorkerDependencies = forall dState. WorkerDependencies
  { getMarloweSyncDriver :: IO (Driver (Handshake MarloweSync) dState IO)
  , getMarloweHeaderSyncDriver :: IO (Driver (Handshake MarloweHeaderSync) dState IO)
  , getMarloweQueryDriver :: IO (Driver (Handshake MarloweQuery) dState IO)
  , getTxJobDriver :: IO (Driver (Handshake (Job MarloweTxCommand)) dState IO)
  , connector :: SomeServerConnector MarloweServer IO
  }

worker :: WorkerDependencies -> IO ()
worker WorkerDependencies{..} = runSomeConnector connector MarloweServer{result = (), ..}
