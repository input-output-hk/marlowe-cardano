{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  , ServerDependencies(..)
  , app
  , server
  , serverWithOpenAPI
  ) where

import Control.Concurrent.Component
import Control.Monad.IO.Unlift (withRunInIO)
import Control.Monad.Reader (runReaderT)
import Data.Void (Void)
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.Monad (AppEnv(..), AppM(..))
import qualified Language.Marlowe.Runtime.Web.Server.OpenAPI as OpenAPI
import Language.Marlowe.Runtime.Web.Server.REST (ApiSelector)
import qualified Language.Marlowe.Runtime.Web.Server.REST as REST
import Language.Marlowe.Runtime.Web.Server.SyncClient (SyncClient(..), SyncClientDependencies(..), syncClient)
import Language.Marlowe.Runtime.Web.Server.TxClient (TxClient(..), TxClientDependencies(..), txClient)
import Network.Protocol.Connection (SomeClientConnector)
import qualified Network.Wai as WAI
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)
import Observe.Event.Class (EventT, backend, runEventT, withModifiedBackend)
import Observe.Event.DSL (SelectorField(Inject), SelectorSpec(SelectorSpec))
import Observe.Event.Explicit (EventBackend, injectSelector, narrowEventBackend)
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Observe.Event.Wai (ServeRequest, application, renderServeRequest)
import qualified Observe.Event.Wai as EvWai
import Servant hiding (Server, respond)

type APIWithOpenAPI = OpenAPI.API :<|>  Web.API

apiWithOpenApi :: Proxy APIWithOpenAPI
apiWithOpenApi = Proxy

serverWithOpenAPI
  :: EventBackend IO r ApiSelector
  -> ServerT APIWithOpenAPI (AppM r)
serverWithOpenAPI eb = OpenAPI.server :<|> REST.server eb

serveAppM
  :: HasServer api '[]
  => Proxy api
  -> AppEnv r
  -> ServerT api (AppM r)
  -> Application
serveAppM api env = serve api . hoistServer api (flip runReaderT env . runAppM)

corsMiddleware :: Bool -> WAI.Middleware
corsMiddleware accessControlAllowOriginAll =
  if accessControlAllowOriginAll
  then do
    let
      policy = simpleCorsResourcePolicy
        { corsRequestHeaders =
          ["Content-Type"
          , "Range"
          , "Accept"
          , "X-Change-Address"
          , "X-Address"
          , "X-Collateral-UTxO"
          ]
        , corsExposedHeaders = Just ["*"]
        , corsMethods = ["GET", "POST", "PUT", "OPTIONS", "DELETE"]
        }
    cors (const $ Just policy)
  else id

compile $ SelectorSpec "server"
  [ ["run", "server"] ≔ ''Void
  , "http" ≔ Inject ''ServeRequest
  , "api" ≔ Inject ''ApiSelector
  ]

app :: Bool -> Bool -> AppEnv r -> EvWai.Application (EventT IO) r ServerSelector
app openAPIEnabled accessControlAllowOriginAll env req handleRes = withRunInIO $ \runInIO -> do
    eb <- runInIO $ withModifiedBackend (narrowEventBackend $ injectSelector Api) backend
    mkApp eb req (runInIO . handleRes)
  where
    mkApp
      | openAPIEnabled = corsMiddleware accessControlAllowOriginAll . serveAppM apiWithOpenApi env . serverWithOpenAPI
      | otherwise = corsMiddleware accessControlAllowOriginAll . serveAppM Web.api env . REST.server

instance DefaultRenderSelectorJSON ServeRequest where
  defaultRenderSelectorJSON = renderServeRequest

data ServerDependencies r = ServerDependencies
  { openAPIEnabled :: Bool
  , accessControlAllowOriginAll :: Bool
  , runApplication :: Application -> IO ()
  , connector :: SomeClientConnector MarloweRuntimeClient IO
  , eventBackend :: EventBackend IO r ServerSelector
  }

{- Architecture notes:
    The web server runs multiple parallel worker processes. If any of them crash,
    the whole application crashes.

    The web server (built with servant-server) runs in a `ReaderT` monad that has
    access to some resources from the other worker processes.

    Application logging is done using the eventuo11y library, which each worker
    process having its own event backend injected via its parameters.
-}

server :: Component IO (ServerDependencies r) ()
server = proc ServerDependencies{..} -> do
  TxClient{..} <- txClient -< TxClientDependencies
    { connector
    }
  SyncClient{..} <- syncClient -< SyncClientDependencies
    { connector
    , lookupTempContract
    , lookupTempTransaction
    , lookupTempWithdrawal
    }
  webServer -< WebServerDependencies
    { env = AppEnv
        { _loadContractHeaders = loadContractHeaders
        , _loadContract = loadContract
        , _loadTransactions = loadTransactions
        , _loadTransaction = loadTransaction
        , _loadWithdrawals = loadWithdrawals
        , _loadWithdrawal = loadWithdrawal
        , _createContract = createContract
        , _applyInputs = applyInputs
        , _withdraw = withdraw
        , _submitContract = submitContract
        , _submitTransaction = submitTransaction
        , _submitWithdrawal = submitWithdrawal
        }
    , eventBackend
    , openAPIEnabled
    , accessControlAllowOriginAll
    , runApplication
    }

data WebServerDependencies r = WebServerDependencies
  { env :: AppEnv r
  , eventBackend :: EventBackend IO r ServerSelector
  , openAPIEnabled :: Bool
  , accessControlAllowOriginAll :: Bool
  , runApplication :: Application -> IO ()
  }

webServer :: Component IO (WebServerDependencies r) ()
webServer = component_ \WebServerDependencies{..} -> do
  app' <- flip runEventT eventBackend
    $ application (injectSelector Http)
    $ app openAPIEnabled accessControlAllowOriginAll env
  runApplication app'
