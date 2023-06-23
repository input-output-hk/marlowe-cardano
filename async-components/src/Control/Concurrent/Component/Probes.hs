{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Control.Concurrent.Component.Probes where

import Colog (Message, WithLog)
import Control.Concurrent.Component
import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Network.Wai.Handler.Warp (Port, run)
import Servant (Get, Handler, JSON, NoContent (NoContent), Server, err500, throwError, type (:<|>) ((:<|>)), type (:>))
import Servant.Client (ClientM, client)
import Servant.Server (serve)
import UnliftIO (MonadUnliftIO)

type ProbeApi =
  "live" :> Probe
    :<|> "ready" :> Probe
    :<|> "startup" :> Probe

type Probe = Get '[JSON] NoContent

api :: Proxy ProbeApi
api = Proxy

getLive, getReady, getStartup :: ClientM NoContent
getLive :<|> getReady :<|> getStartup = client api

data Probes = Probes
  { liveness :: IO Bool
  , readiness :: IO Bool
  , startup :: IO Bool
  }

server :: Probes -> Server ProbeApi
server Probes{..} = handler liveness :<|> handler readiness :<|> handler startup

handler :: IO Bool -> Handler NoContent
handler probe = do
  pass <- liftIO probe
  if pass then pure NoContent else throwError err500

data ProbeServerDependencies = ProbeServerDependencies
  { probes :: Probes
  , port :: Port
  }

probeServer :: (WithLog env Message m, MonadUnliftIO m) => Component m ProbeServerDependencies ()
probeServer = component_ "probe-server" \ProbeServerDependencies{..} ->
  liftIO $ run port $ serve api (server probes)
