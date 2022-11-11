{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines the top-level aggregate process (HTTP server and
-- worker processes) for running the web server.

module Language.Marlowe.Runtime.Web.Server
  ( APIWithOpenAPI
  , module Language.Marlowe.Runtime.Web.Server.Flags
  , Server(..)
  , ServerDependencies(..)
  , app
  , mkServer
  , serverWithOpenAPI
  ) where

import Control.Concurrent.Async (mapConcurrently_)
import Control.Monad.Reader (runReaderT)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Server.ContractHeaderIndexer
  (ContractHeaderIndexer(..), ContractHeaderIndexerDependencies(..), mkContractHeaderIndexer)
import Language.Marlowe.Runtime.Web.Server.Flags
import Language.Marlowe.Runtime.Web.Server.Monad (AppEnv(..), AppM(..))
import qualified Language.Marlowe.Runtime.Web.Server.OpenAPI as OpenAPI
import qualified Language.Marlowe.Runtime.Web.Server.REST as REST
import Servant hiding (Server)

type APIWithOpenAPI = OpenAPI.API :<|>  Web.API

apiWithOpenApi :: Proxy APIWithOpenAPI
apiWithOpenApi = Proxy

serverWithOpenAPI :: ServerT APIWithOpenAPI AppM
serverWithOpenAPI = OpenAPI.server :<|> REST.server

serveAppM :: HasServer api '[] => Proxy api -> AppEnv -> ServerT api AppM -> Application
serveAppM api env = serve api . hoistServer api (flip runReaderT env . runAppM)

app :: AppEnv -> Bool -> Application
app env True = serveAppM apiWithOpenApi env serverWithOpenAPI
app env False = serveAppM Web.api env REST.server

data ServerDependencies = ServerDependencies
  { openAPIEnabled :: Bool
  , runApplication :: Application -> IO ()
  , runMarloweHeaderSyncClient :: forall a. MarloweHeaderSyncClient IO a -> IO a
  }

newtype Server = Server
  { runServer :: IO ()
  }

mkServer :: ServerDependencies -> IO Server
mkServer ServerDependencies{..} = do
  ContractHeaderIndexer{..} <- mkContractHeaderIndexer ContractHeaderIndexerDependencies{..}
  let
    env = AppEnv
      { _loadContractHeaders = loadContractHeaders
      }
  pure Server
    { runServer = mapConcurrently_ id
      [ runApplication $ app env openAPIEnabled
      , runContractHeaderIndexer
      ]
    }
