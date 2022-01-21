-- The types are defined separated from the MarloweApp to avoid this circular dependency
-- Capability.PlutusApps.MarloweApp -> AppM -> Env -> Capability.PlutusApps.MarloweApp
module Capability.PlutusApps.MarloweApp.Types
  ( EndpointName
  , MarloweEndpointResponse
  , MarloweInput
  , MarloweSlotRange
  , MarloweAppState
  , EndpointMutex
  , class HasMarloweAppEndpointMutex
  , marloweAppEndpointMutex
  ) where

import Prologue

import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Effect.AVar (AVar)
import Marlowe.Semantics (Input, Slot)
import Language.Marlowe.Client
  ( EndpointResponse
  , MarloweEndpointResult
  , MarloweError
  )

type EndpointName
  = String

type MarloweEndpointResponse = EndpointResponse MarloweEndpointResult
  MarloweError

type MarloweInput
  = Tuple MarloweSlotRange (Array Input)

type MarloweSlotRange
  = Tuple Slot Slot

type MarloweAppState
  = Maybe MarloweEndpointResponse

-- The plutus contracts can have their endpoints active or inactive. We use
-- this object with Mutex to avoid calling an inactive endpoint and to keep
-- track of the different requests.
type EndpointMutex
  =
  { create :: AVar Unit
  , applyInputs :: AVar Unit
  , redeem :: AVar Unit
  -- For each request we fire, we store in a queue the tuple of the
  -- request id and a mutex to wait for the response. We use an array
  -- instead of a Map because we only want to keep a limited number of
  -- requests.
  , requests :: AVar (Array (UUID /\ AVar MarloweEndpointResponse))
  }

class HasMarloweAppEndpointMutex env where
  marloweAppEndpointMutex :: env -> EndpointMutex
