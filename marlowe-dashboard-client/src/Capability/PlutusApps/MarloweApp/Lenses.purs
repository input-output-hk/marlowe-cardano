module Capability.PlutusApps.MarloweApp.Lenses where

import Prologue

import Capability.PlutusApps.MarloweApp.Types
  ( class HasMarloweAppEndpointMutex
  , EndpointMutex
  , LastResult
  , marloweAppEndpointMutex
  )
import Data.Lens (Getter', Lens', to)
import Data.Lens.Record (prop)
import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Effect.AVar (AVar)
import Type.Proxy (Proxy(..))

_marloweAppEndpointMutex
  :: forall env. HasMarloweAppEndpointMutex env => Getter' env EndpointMutex
_marloweAppEndpointMutex = to marloweAppEndpointMutex

_redeem :: Lens' EndpointMutex (AVar Unit)
_redeem = prop (Proxy :: _ "redeem")

_create :: Lens' EndpointMutex (AVar Unit)
_create = prop (Proxy :: _ "create")

_applyInputs :: Lens' EndpointMutex (AVar Unit)
_applyInputs = prop (Proxy :: _ "applyInputs")

_requests :: Lens' EndpointMutex (AVar (Array (UUID /\ AVar LastResult)))
_requests = prop (Proxy :: _ "requests")
