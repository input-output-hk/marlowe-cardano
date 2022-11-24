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
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.ContractHeaderIndexer
  ( ContractHeaderIndexer(..)
  , ContractHeaderIndexerDependencies(..)
  , ContractHeaderIndexerSelector
  , mkContractHeaderIndexer
  )
import Language.Marlowe.Runtime.Web.Server.HistoryClient
  (HistoryClient(..), HistoryClientDependencies(..), HistoryClientSelector, mkHistoryClient)
import Language.Marlowe.Runtime.Web.Server.Monad (AppEnv(..), AppM(..))
import qualified Language.Marlowe.Runtime.Web.Server.OpenAPI as OpenAPI
import Language.Marlowe.Runtime.Web.Server.REST (ApiSelector)
import qualified Language.Marlowe.Runtime.Web.Server.REST as REST
import Language.Marlowe.Runtime.Web.Server.TxClient (TxClient(..), TxClientDependencies(..), mkTxClient)
import Network.Protocol.Driver (RunClient)
import Network.Protocol.Job.Client (JobClient)
import Observe.Event (EventBackend, hoistEventBackend, narrowEventBackend)
import Observe.Event.BackendModification (modifyEventBackend, setAncestor)
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
  :: EventBackend (AppM r) r ApiSelector
  -> ServerT APIWithOpenAPI (AppM r)
serverWithOpenAPI eb = OpenAPI.server :<|> REST.server eb

serveAppM
  :: HasServer api '[]
  => Proxy api
  -> AppEnv r
  -> ServerT api (AppM r)
  -> Application
serveAppM api env = serve api . hoistServer api (flip runReaderT env . runAppM)

app :: Bool -> AppEnv r -> EventBackend (AppM r) r ApiSelector -> Application
app True env = serveAppM apiWithOpenApi env . serverWithOpenAPI
app False env = serveAppM Web.api env . REST.server

instance DefaultRenderSelectorJSON ServeRequest where
  defaultRenderSelectorJSON = renderServeRequest

compile $ SelectorSpec "server"
  [ ["run", "server"] ≔ ''Void
  , "http" ≔ Inject ''ServeRequest
  , "api" ≔ Inject ''ApiSelector
  , ["contract", "indexer"] ≔ Inject ''ContractHeaderIndexerSelector
  , ["history"] ≔ Inject ''HistoryClientSelector
  ]

data ServerDependencies r = ServerDependencies
  { openAPIEnabled :: Bool
  , runApplication :: Application -> IO ()
  , runMarloweHeaderSyncClient :: RunClient IO MarloweHeaderSyncClient
  , runMarloweSyncClient :: RunClient IO MarloweSyncClient
  , runTxJobClient :: RunClient IO (JobClient MarloweTxCommand)
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
  TxClient{..} <- mkTxClient TxClientDependencies
    { runTxJobClient
    }
  ContractHeaderIndexer{..} <- mkContractHeaderIndexer ContractHeaderIndexerDependencies
    { runMarloweHeaderSyncClient
    , getTempContracts
    , eventBackend = narrowEventBackend ContractIndexer eventBackend
    }
  HistoryClient{..} <- mkHistoryClient HistoryClientDependencies
    { runMarloweSyncClient
    , lookupTempContract
    , eventBackend = narrowEventBackend History eventBackend
    }
  let
    env = AppEnv
      { _loadContractHeaders = loadContractHeaders
      , _loadContract = loadContract
      , _loadTransactions = loadTransactions
      , _createContract = createContract
      }
    httpBackend = hoistEventBackend liftIO $ narrowEventBackend Api eventBackend
    app' = application (narrowEventBackend Http eventBackend) $
      app openAPIEnabled env . (`modifyEventBackend`  httpBackend) . setAncestor
  pure Server
    { runServer = mapConcurrently_ @[] id
        [ runApplication app'
        , runContractHeaderIndexer
        ]
    }
