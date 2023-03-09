{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}


module Language.Marlowe.Runtime.App.Run
  ( runChainSeekClient
  , runClientWithConfig
  , runJobClient
  ) where


import Control.Monad.Reader (ask)
import Control.Monad.Trans.Control (liftBaseWith)
import Control.Monad.Trans.Reader (ReaderT(..))
import Language.Marlowe.Runtime.App.Types (Client(..), Config(..), Services(..))
import Language.Marlowe.Runtime.ChainSync.Api (RuntimeChainSeekClient)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer, hoistChainSeekClient)
import Network.Protocol.Driver (runConnector, tcpClient)
import Network.Protocol.Handshake.Client (handshakeClientConnector)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, jobClientPeer)


runJobClient
  :: (Services IO -> JobClient q IO a -> IO a)
  -> JobClient q Client a
  -> Client a
runJobClient job client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> job services $ hoistJobClient runInBase client


runChainSeekClient
  :: (Services IO -> RuntimeChainSeekClient IO a -> IO a)
  -> RuntimeChainSeekClient Client a
  -> Client a
runChainSeekClient seek client =
  do
    services <- Client ask
    liftBaseWith $ \runInBase -> seek services $ hoistChainSeekClient runInBase client


runClientWithConfig
  :: Config
  -> Client a
  -> IO a
runClientWithConfig Config{..} client = runReaderT (connectToMarloweRuntime runtimeHost runtimePort (runClient client)) Services
  { runChainSeekCommandClient = runConnector $ handshakeClientConnector $ tcpClient chainSeekHost chainSeekCommandPort jobClientPeer
  , runChainSeekSyncClient = runConnector $ handshakeClientConnector $ tcpClient chainSeekHost chainSeekSyncPort chainSeekClientPeer
  }
