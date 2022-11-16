{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the top-level aggregate process (HTTP server and
-- worker processes) for running the web server.

module Language.Marlowe.Runtime.Web.Server
  ( APIWithOpenAPI
  , Server(..)
  , ServerDependencies(..)
  , app
  , mkServer
  , serverWithOpenAPI
  ) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Concurrent.STM (STM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (runReaderT)
import Data.Void (Void)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.ContractHeaderIndexer
  ( ContractHeaderIndexer(..)
  , ContractHeaderIndexerDependencies(..)
  , ContractHeaderIndexerSelector
  , mkContractHeaderIndexer
  )
import Language.Marlowe.Runtime.Web.Server.Monad (AppEnv(..), AppM(..))
import qualified Language.Marlowe.Runtime.Web.Server.OpenAPI as OpenAPI
import Language.Marlowe.Runtime.Web.Server.REST (ApiSelector)
import qualified Language.Marlowe.Runtime.Web.Server.REST as REST
import Observe.Event (EventBackend, hoistEventBackend, narrowEventBackend)
import Observe.Event.BackendModification (EventBackendModifiers, setAncestor)
import Observe.Event.DSL (SelectorField(Inject), SelectorSpec(SelectorSpec))
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Observe.Event.Wai (ServeRequest, application, renderServeRequest)
import Servant hiding (Server)

type APIWithOpenAPI = OpenAPI.API :<|>  Web.API

apiWithOpenApi :: Proxy APIWithOpenAPI
apiWithOpenApi = Proxy

serverWithOpenAPI
  :: EventBackend AppM r ApiSelector
  -> EventBackendModifiers r r'
  -> ServerT APIWithOpenAPI AppM
serverWithOpenAPI eventBackend mods = OpenAPI.server :<|> REST.server eventBackend mods

serveAppM
  :: HasServer api '[]
  => Proxy api
  -> AppEnv
  -> ServerT api AppM
  -> Application
serveAppM api env = serve api . hoistServer api (flip runReaderT env . runAppM)

app :: Bool -> AppEnv -> EventBackend AppM r ApiSelector -> EventBackendModifiers r r' -> Application
app True env eventBackend = serveAppM apiWithOpenApi env . serverWithOpenAPI eventBackend
app False env eventBackend = serveAppM Web.api env . REST.server eventBackend

instance DefaultRenderSelectorJSON ServeRequest where
  defaultRenderSelectorJSON = renderServeRequest

compile $ SelectorSpec "server"
  [ ["run", "server"] ≔ ''Void
  , "http" ≔ Inject ''ServeRequest
  , "api" ≔ Inject ''ApiSelector
  , ["contract", "indexer"] ≔ Inject ''ContractHeaderIndexerSelector
  ]

data ServerDependencies r = ServerDependencies
  { openAPIEnabled :: Bool
  , runApplication :: Application -> IO ()
  , runMarloweHeaderSyncClient :: forall a. MarloweHeaderSyncClient IO a -> IO a
  , eventBackend :: EventBackend IO r ServerSelector
  }

newtype Server = Server
  { runServer :: IO ()
  }

{- Architecture notes:
    The web server runs multiple parallel worker processes. If any of them crash,
    the whole application crashes.

    The web server (built with servant-server) runs in a `ReaderT` monad that has
    access to some resources from the other worker processes.

    Application logging is done using the eventuo11y library, which each worker
    process having its own event backend injected via its parameters.
-}

mkServer :: ServerDependencies r -> STM Server
mkServer ServerDependencies{..} = do
  ContractHeaderIndexer{..} <- mkContractHeaderIndexer ContractHeaderIndexerDependencies
    { runMarloweHeaderSyncClient
    , eventBackend = narrowEventBackend ContractIndexer eventBackend
    }
  let
    env = AppEnv
      { _loadContractHeaders = loadContractHeaders
      }
    httpBackend = hoistEventBackend liftIO $ narrowEventBackend Api eventBackend
    app' = application (narrowEventBackend Http eventBackend) $ app openAPIEnabled env httpBackend . setAncestor
  pure Server
    { runServer = mapConcurrently_ @[] id
        [ runApplication app'
        , runContractHeaderIndexer
        ]
    }
