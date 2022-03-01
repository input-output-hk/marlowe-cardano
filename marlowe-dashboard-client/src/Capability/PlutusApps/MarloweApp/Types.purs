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

type EndpointName
  = String

type MarloweEndpointResponse = EndpointResponse MarloweEndpointResult
  MarloweError

type MarloweAppState
  = Maybe MarloweEndpointResponse

type PendingResults = AVar (Array (UUID /\ (AVar MarloweEndpointResponse)))
