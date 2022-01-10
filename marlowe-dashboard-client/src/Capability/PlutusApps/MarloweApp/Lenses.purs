module Capability.PlutusApps.MarloweApp.Lenses where

import Prologue
import Capability.PlutusApps.MarloweApp.Types
  ( EndpointMutex
  , LastResult
  , MarloweAppEndpointMutexEnv
  )
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Type.Proxy (Proxy(..))
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Effect.AVar (AVar)

_marloweAppEndpointMutex
  :: forall a. Lens' (MarloweAppEndpointMutexEnv a) EndpointMutex
_marloweAppEndpointMutex = prop (Proxy :: _ "marloweAppEndpointMutex")

_redeem :: Lens' EndpointMutex (AVar Unit)
_redeem = prop (Proxy :: _ "redeem")

_create :: Lens' EndpointMutex (AVar Unit)
_create = prop (Proxy :: _ "create")

_applyInputs :: Lens' EndpointMutex (AVar Unit)
_applyInputs = prop (Proxy :: _ "applyInputs")

_requests :: Lens' EndpointMutex (AVar (Array (UUID /\ AVar LastResult)))
_requests = prop (Proxy :: _ "requests")
