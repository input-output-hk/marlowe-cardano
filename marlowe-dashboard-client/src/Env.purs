module Env where

import Prologue

import Capability.PlutusApps.MarloweApp.Types
  ( MarloweEndpointResponse
  )
import Capability.PlutusApps.MarloweApp.Types as MarloweApp
import Control.Logger.Effect (Logger)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Newtype (class Newtype)
import Data.Time.Duration (Milliseconds)
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Effect.AVar (AVar)
import Halogen (SubscriptionId)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , CombinedWSStreamToServer
  )
import Type.Proxy (Proxy(..))
import WebSocket.Support (WebSocketManager) as WS

-- Application enviroment configuration
newtype Env = Env
  {
    -- This AVar helps to solve a concurrency problem in the contract carousel subscriptions.
    -- See notes in [Contract.State(unsubscribeFromSelectCenteredStep)]
    -- There are two reasons why this is stored in the `Env` rather than the Contract.State:
    -- 1. There are multiple Contract.State (one per each contract) but only one carousel at a time.
    --    Sharing the subscription makes sense in that regard.
    -- 2. We need to be inside the Effect/Aff monad in order to create an AVar, and most of the state
    --    creation functions didn't require that, so it seemed wrong to lift several functions into Effect.
    --    In contrast, the Env is created in Main, where we already have access to Effect
    contractStepCarouselSubscription :: AVar SubscriptionId
  , logger :: Logger String
  -- See note on Capability.PlutusApps.MarloweApp.Types
  , marloweEndpoints :: MarloweApp.Endpoints
  -- For each request we fire, we store in a queue the tuple of the
  -- request id and a mutex to wait for the response. We use an array
  -- instead of a Map because we only want to keep a limited number of
  -- requests.
  , pendingRequests :: AVar (Array (UUID /\ AVar MarloweEndpointResponse))
  , wsManager :: WebSocketManager
  , pollingInterval :: Milliseconds
  }

derive instance newtypeEnv :: Newtype Env _

type WebSocketManager
  = WS.WebSocketManager CombinedWSStreamToClient CombinedWSStreamToServer

_pollingInterval :: Lens' Env Milliseconds
_pollingInterval = _Newtype <<< prop (Proxy :: _ "pollingInterval")

_pendingRequests :: Lens'
  Env
  (AVar (Array (UUID /\ AVar MarloweEndpointResponse)))
_pendingRequests = _Newtype <<< prop (Proxy :: _ "pendingRequests")

_marloweEndpoints :: Lens' Env MarloweApp.Endpoints
_marloweEndpoints = _Newtype <<< prop (Proxy :: _ "marloweEndpoints")
