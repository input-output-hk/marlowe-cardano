-- The types are defined separated from the MarloweApp to avoid this circular dependency
-- Capability.PlutusApps.MarloweApp -> AppM -> Env -> Capability.PlutusApps.MarloweApp
module Capability.PlutusApps.MarloweApp.Types where

import Prologue

import Data.Tuple.Nested (type (/\))
import Data.UUID.Argonaut (UUID)
import Effect.AVar (AVar)
import Language.Marlowe.Client
  ( EndpointResponse
  , MarloweEndpointResult
  , MarloweError
  )
import Marlowe.Semantics (Input, Slot)

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
-- this object with semephores to avoid calling an inactive endpoint.
type Endpoints =
  { create :: AVar Unit
  , applyInputs :: AVar Unit
  , redeem :: AVar Unit
  }

type PendingResults = AVar (Array (UUID /\ (AVar MarloweEndpointResponse)))
