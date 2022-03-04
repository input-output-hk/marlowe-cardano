module Env where

import Prologue

import Affjax (Response)
import Affjax as Affjax
import Control.Logger.Effect (Logger)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Newtype (class Newtype)
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Effect (Effect)
import Effect.AVar (AVar)
import Effect.Aff (Aff)
import Halogen (SubscriptionId)
import Halogen.Subscription (Emitter, Listener, Subscription)
import LocalStorage (Key)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweParams)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , CombinedWSStreamToServer
  )
import Type.Proxy (Proxy(..))
import WebSocket.Support (FromSocket)

-- A two-layer mapping of semaphores. The keys of the outer Map correspond to
-- the app IDs of the different plutus apps, the keys of the inner Map
-- correspond to the endpoint names of the endpoints within that app, and the
-- values of the inner map are AVars which are used to conrol when that
-- endpoint is available (when the AVar is empty, it is unavailable, and
-- prospective clients of that endpoint will need to wait until it becomes
-- available again).
type EndpointSemaphores = Map PlutusAppId (Map String (AVar Unit))

type PollingSources =
  { walletRegular :: Emitter Unit
  , walletSync :: Emitter Unit
  }

type Sources =
  { pabWebsocket :: Emitter (FromSocket CombinedWSStreamToClient)
  , clock :: Emitter Unit
  , polling :: PollingSources
  }

type Sinks =
  { pabWebsocket :: Listener CombinedWSStreamToServer
  }

type LocalStorageApi =
  { removeItem :: Key -> Effect Unit
  , getItem :: Key -> Effect (Maybe String)
  , setItem :: Key -> String -> Effect Unit
  }

-- Newtype wrapper for this callback because PureScript doesn't like pualified
-- types to appear in records.
newtype HandleRequest = HandleRequest
  (forall a. Affjax.Request a -> Aff (Either Affjax.Error (Response a)))

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
  , endpointSemaphores :: AVar EndpointSemaphores
  , createListeners ::
      AVar (Map UUID (Maybe Subscription /\ Listener MarloweParams))
  , applyInputListeners :: AVar (Map UUID (Maybe Subscription /\ Listener Unit))
  , redeemListeners :: AVar (Map UUID (Maybe Subscription /\ Listener Unit))
  -- | All the outbound communication channels to the outside world
  , sinks :: Sinks
  -- | All the inbound communication channels from the outside world
  , sources :: Sources
  -- | This allows us to inject a custom HTTP request effect, overriding the
  -- | default one for testing or global extension purposes.
  , handleRequest :: HandleRequest
  , localStorage :: LocalStorageApi
  }

derive instance newtypeEnv :: Newtype Env _

_createListeners :: Lens' Env
  (AVar (Map UUID (Maybe Subscription /\ Listener MarloweParams)))
_createListeners = _Newtype <<< prop (Proxy :: _ "createListeners")

_applyInputListeners :: Lens' Env
  (AVar (Map UUID (Maybe Subscription /\ Listener Unit)))
_applyInputListeners = _Newtype <<< prop (Proxy :: _ "applyInputListeners")

_redeemListeners :: Lens' Env
  (AVar (Map UUID (Maybe Subscription /\ Listener Unit)))
_redeemListeners = _Newtype <<< prop (Proxy :: _ "redeemListeners")

_endpointSemaphores :: Lens' Env (AVar EndpointSemaphores)
_endpointSemaphores = _Newtype <<< prop (Proxy :: _ "endpointSemaphores")

_sources :: Lens' Env Sources
_sources = _Newtype <<< prop (Proxy :: _ "sources")

_sinks :: Lens' Env Sinks
_sinks = _Newtype <<< prop (Proxy :: _ "sinks")

_handleRequest :: Lens' Env HandleRequest
_handleRequest = _Newtype <<< prop (Proxy :: _ "handleRequest")

_localStorage :: Lens' Env LocalStorageApi
_localStorage = _Newtype <<< prop (Proxy :: _ "localStorage")
