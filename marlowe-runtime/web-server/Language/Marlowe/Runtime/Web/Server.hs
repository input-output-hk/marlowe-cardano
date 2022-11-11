{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.Web.Server
  ( API
  , module Language.Marlowe.Runtime.Web.Server.Flags
  , Server(..)
  , ServerDependencies(..)
  , app
  , mkServer
  , server
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

type family API openAPIFlag where
  API Enabled = OpenAPI.API :<|> Web.API
  API Disabled = Web.API

apiWithOpenApi :: Proxy (API Enabled)
apiWithOpenApi = Proxy

apiWithoutOpenApi :: Proxy (API Disabled)
apiWithoutOpenApi = Proxy

server :: Flag openAPIFlag -> ServerT (API openAPIFlag) AppM
server = \case
  Enabled -> OpenAPI.server :<|> REST.server
  Disabled -> REST.server

app :: AppEnv -> Flag openAPIFlag -> Application
app env Enabled =
  serve apiWithOpenApi $ hoistServer apiWithOpenApi (flip runReaderT env . runAppM) $ server Enabled
app env Disabled =
  serve apiWithoutOpenApi $ hoistServer apiWithoutOpenApi (flip runReaderT env . runAppM) $ server Disabled

data ServerDependencies = forall openAPI. ServerDependencies
  { openAPI :: Flag openAPI
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
      [ runApplication $ app env Enabled
      , runContractHeaderIndexer
      ]
    }
