-- The types are defined separated from the MarloweApp to avoid this circular dependency
-- Capability.PlutusApps.MarloweApp -> AppM -> Env -> Capability.PlutusApps.MarloweApp
module Capability.PlutusApps.MarloweApp.Types
  ( LastResult(..)
  , EndpointName
  , MarloweError(..)
  , MarloweInput
  , MarloweSlotRange
  , MarloweAppState
  , EndpointMutex
  , MarloweAppEndpointMutexEnv
  ) where

import Prologue
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson as E
import Data.Argonaut.Encode.Aeson ((>/\<))
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Tuple (uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Effect.AVar (AVar)
import Marlowe.Semantics (Input, MarloweData, Slot, TransactionError)
import Plutus.Contract.StateMachine (InvalidTransition, SMContractError)
import Wallet.Types (ContractError)

type EndpointName = String

-- The Plutus contract state keeps track of the result of the last action. This is needed because
-- the PAB needs to return inmediatly and the result might take a while to compute.
data LastResult
  = OK UUID EndpointName
  | SomeError UUID EndpointName MarloweError
  | Unknown

derive instance genericLastResult :: Generic LastResult _

instance encodeLastResult :: EncodeJson LastResult where
  encodeJson (OK uuid epName) = E.encodeTagged "OK" (uuid /\ epName) E.value
  encodeJson (SomeError uuid epName me) =
    E.encodeTagged
      "SomeError"
      (encodeJson uuid /\ encodeJson epName /\ encodeJson me)
      (E.tuple (E.value >/\< E.value >/\< E.value))
  encodeJson Unknown = encodeJson { tag: "Unknown" }

instance decodeJsonLastResult :: DecodeJson LastResult where
  decodeJson =
    D.decode
      $ D.sumType "LastResult"
      $ Map.fromFoldable
          [ "OK" /\ D.content (uncurry OK <$> D.value)
          , "SomeError" /\ D.content
              (D.tuple $ SomeError </$\> D.value </*\> D.value </*\> D.value)
          , "Unknown" /\ D.content (Unknown <$ D.null)
          ]

data MarloweError
  = StateMachineError SMContractError
  -- ^ can arise when applying inputs if:
  --     (a) there's a duplicate marlowe contract (which could theoretially happen if someone is deliberately trying to break things)
  --     (b) the contract doesn't exist or has already closed
  --     (c) you don't have enough money
  | TransitionError (InvalidTransition MarloweData MarloweInput)
  -- ^ can arise when applying inputs if:
  --     (a) you don't have the right role token (the frontend should rule this out anyway)
  --     (b) someone else made the move first
  --     (c) you don't have enough money
  | MarloweEvaluationError TransactionError
  -- ^ can arise when applying inputs (should just match the frontend semantics)
  | OtherContractError ContractError
  -- ^ can arise when creating a contract if you don't provide pubKeys for all the roles (the frontend should rule this out anyway)
  -- note `ContractError` is more general, but we only use this here for its `OtherError` constructor, and in this one specific case
  | RolesCurrencyError ContractError

-- ^ can arise when creating a contract if you don't have enough money
derive instance eqMarloweError :: Eq MarloweError

derive instance genericMarloweError :: Generic MarloweError _

instance encodeJsonMarloweError :: EncodeJson MarloweError where
  encodeJson (StateMachineError err) = E.encodeTagged "StateMachineError" err
    E.value
  encodeJson (TransitionError err) = E.encodeTagged "TransactionError" err
    E.value
  encodeJson (MarloweEvaluationError err) = E.encodeTagged
    "MarloweEvaluationError"
    err
    E.value
  encodeJson (OtherContractError err) = E.encodeTagged "OtherContractError" err
    E.value
  encodeJson (RolesCurrencyError err) = E.encodeTagged "RolesCurrencyError" err
    E.value

instance decodeJsonMarloweError :: DecodeJson MarloweError where
  decodeJson =
    D.decode
      $ D.sumType "MarloweError"
      $ Map.fromFoldable
          [ "StateMachineError" /\ D.content (StateMachineError <$> D.value)
          , "TransitionError" /\ D.content (TransitionError <$> D.value)
          , "MarloweEvaluationError" /\ D.content
              (MarloweEvaluationError <$> D.value)
          , "OtherContractError" /\ D.content (OtherContractError <$> D.value)
          , "RolesCurrencyError" /\ D.content (RolesCurrencyError <$> D.value)
          ]

type MarloweInput
  = Tuple MarloweSlotRange (Array Input)

type MarloweSlotRange
  = Tuple Slot Slot

-- We use an alias because we could later on add more info to the state
type MarloweAppState
  = LastResult

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
  , requests :: AVar (Array (UUID /\ AVar LastResult))
  }

type MarloweAppEndpointMutexEnv env =
  { marloweAppEndpointMutex :: EndpointMutex | env }
