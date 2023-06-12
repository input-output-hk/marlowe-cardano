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

{-# LANGUAGE MultiParamTypeClasses #-}

-- | This module defines the top-level aggregate process (HTTP server and
-- worker processes) for running the web server.

module Language.Marlowe.Runtime.Web.Server
  ( APIWithOpenAPI
  , ServeRequest(..)
  , ServeRequestField(..)
  , ServerDependencies(..)
  , ServerSelector(..)
  , server
  , serverWithOpenAPI
  ) where

import Control.Concurrent.Component
import Control.Monad.Event.Class
import Control.Monad.IO.Unlift (liftIO, withRunInIO)
import Control.Monad.Reader (ReaderT(ReaderT), runReaderT)
import Control.Monad.Trans.Marlowe (MarloweTracedContext(..))
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.Monad (AppEnv(..), AppM(..), BackendM(BackendM))
import qualified Language.Marlowe.Runtime.Web.Server.OpenAPI as OpenAPI
import qualified Language.Marlowe.Runtime.Web.Server.REST as REST
import Language.Marlowe.Runtime.Web.Server.SyncClient
  ( LoadContract
  , LoadContractHeaders
  , LoadTransaction
  , LoadTransactions
  , LoadWithdrawal
  , LoadWithdrawals
  , SyncClient(..)
  , SyncClientDependencies(..)
  , syncClient
  )
import Language.Marlowe.Runtime.Web.Server.TxClient
  (ApplyInputs, CreateContract, Submit, TxClient(..), TxClientDependencies(..), Withdraw, txClient)
import Network.Protocol.Connection (SomeConnectorTraced(..))
import Network.Protocol.Driver.Trace (HasSpanContext)
import Network.Protocol.Handshake.Types (Handshake)
import Network.Wai (Request, Response)
import qualified Network.Wai as WAI
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleCorsResourcePolicy)
import Observe.Event (reference)
import Observe.Event.Backend (Event(addField))
import Observe.Event.Explicit (injectSelector)
import Servant hiding (Server, respond)

data ServeRequest f where
  -- We need the request in the selector constructor as well because we need it
  -- for the span name.
  ServeRequest :: Request -> ServeRequest ServeRequestField

data ServeRequestField
  = ReqField Request
  | ResField Response

data ServerSelector transport f where
  Http :: ServeRequest f -> ServerSelector transport f
  RuntimeClient :: transport (Handshake MarloweRuntime) f -> ServerSelector transport f

instance Inject ServeRequest (ServerSelector transport) where
  inject = injectSelector Http

type APIWithOpenAPI = OpenAPI.API :<|>  Web.API

apiWithOpenApi :: Proxy APIWithOpenAPI
apiWithOpenApi = Proxy

serverWithOpenAPI :: ServerT APIWithOpenAPI AppM
serverWithOpenAPI = OpenAPI.server :<|> REST.server

serveAppM
  :: HasServer api '[]
  => Proxy api
  -> AppEnv
  -> ServerT api AppM
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

data ServerDependencies transport r s = ServerDependencies
  { openAPIEnabled :: Bool
  , accessControlAllowOriginAll :: Bool
  , runApplication :: Application -> IO ()
  , marloweTracedContext :: MarloweTracedContext r s (ServerSelector transport) (BackendM r (ServerSelector transport))
  }

{- Architecture notes:
    The web server runs multiple parallel worker processes. If any of them crash,
    the whole application crashes.

    The web server (built with servant-server) runs in a `ReaderT` monad that has
    access to some resources from the other worker processes.

    Application logging is done using the eventuo11y library, which each worker
    process having its own event backend injected via its parameters.
-}

server :: HasSpanContext r => Component (BackendM r (ServerSelector transport)) (ServerDependencies transport r s) ()
server = proc deps@ServerDependencies{marloweTracedContext = MarloweTracedContext{..}} -> do
  TxClient{..} <- txClient -< TxClientDependencies
    { connector = SomeConnectorTraced injector connector
    }
  SyncClient{..} <- syncClient -< SyncClientDependencies
    { connector = SomeConnectorTraced injector connector
    , lookupTempContract
    , lookupTempTransaction
    , lookupTempWithdrawal
    }
  webServer -< case deps of
    ServerDependencies{ ..} ->
      WebServerDependencies
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
        , openAPIEnabled
        , accessControlAllowOriginAll
        , runApplication
        }

data WebServerDependencies r s = WebServerDependencies
  { _loadContractHeaders :: LoadContractHeaders (BackendM r s)
  , _loadContract :: LoadContract (BackendM r s)
  , _loadWithdrawals :: LoadWithdrawals (BackendM r s)
  , _loadWithdrawal :: LoadWithdrawal (BackendM r s)
  , _loadTransactions :: LoadTransactions (BackendM r s)
  , _loadTransaction :: LoadTransaction (BackendM r s)
  , _createContract :: CreateContract (BackendM r s)
  , _withdraw :: Withdraw (BackendM r s)
  , _applyInputs :: ApplyInputs (BackendM r s)
  , _submitContract :: ContractId -> Submit r (BackendM r s)
  , _submitTransaction :: ContractId -> TxId -> Submit r (BackendM r s)
  , _submitWithdrawal :: TxId -> Submit r (BackendM r s)
  , openAPIEnabled :: Bool
  , accessControlAllowOriginAll :: Bool
  , runApplication :: Application -> IO ()
  }

webServer :: Component (BackendM r (ServerSelector transport)) (WebServerDependencies r (ServerSelector transport)) ()
webServer = component_ "web-server" \WebServerDependencies{..} -> withRunInIO \runInIO ->
  -- Observe.Event.Wai does not expose a reference to the ServeRequest field, which we
  -- need because of the asynchronous processing of submit jobs. So, we have to
  -- roll our own version of Observe.Event.Wai.application here. A bonus is
  -- that we do not have to translate to and from EventT and BackendM.
  runApplication \req handleRes ->
    runInIO $ withEventFields (ServeRequest req) [ReqField req] \ev -> do
      _eventBackend <- askBackend
      _logAction <- BackendM $ ReaderT \(_, logAction) -> pure logAction
      let _requestParent = reference ev
      let
        mkApp
          | openAPIEnabled = corsMiddleware accessControlAllowOriginAll $ serveAppM apiWithOpenApi AppEnv{..} serverWithOpenAPI
          | otherwise = corsMiddleware accessControlAllowOriginAll $ serveAppM Web.api AppEnv{..} REST.server
      liftIO $ mkApp req \res -> runInIO do
        addField ev $ ResField res
        liftIO $ handleRes res
