-- This capability exposes the API for the Plutus Aplication called `MarloweApp`, which
-- has it's endpoints defined as the MarloweSchema in Language.Marlowe.Client module.
-- There is one `MarloweApp` per wallet, and it serves as a control app for all
-- the Marlowe contracts that the wallet has a role token.
module Capability.PlutusApps.MarloweApp
  ( class MarloweApp
  , createContract
  , applyInputs
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Bridge (toBack)
import Capability.PAB (invokeEndpoint) as PAB
import Control.Concurrent.EventBus as EventBus
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (asks)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Address (Address)
import Data.Argonaut.Encode (encodeJson)
import Data.Lens (_1, over, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID, genUUID)
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Env (_applyInputBus, _createBus, _redeemBus)
import Language.Marlowe.Client (MarloweError)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( Contract
  , MarloweParams
  , TimeInterval(..)
  , TokenName
  , TransactionInput(..)
  )
import Plutus.PAB.Webserver (Api) as PAB
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (TokenName) as Back
import Servant.PureScript (class MonadAjax)
import Types (AjaxResponse)

class MarloweApp m where
  createContract
    :: PlutusAppId
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse (UUID /\ Aff (Either MarloweError MarloweParams)))
  applyInputs
    :: PlutusAppId
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff (Either MarloweError Unit)))
  -- TODO auto
  -- TODO close
  redeem
    :: PlutusAppId
    -> MarloweParams
    -> TokenName
    -> Address
    -> m (AjaxResponse (Aff (Either MarloweError Unit)))

instance
  ( MonadError Error m
  , MonadAff m
  , MonadRec m
  , MonadAjax PAB.Api m
  ) =>
  MarloweApp (AppM m) where
  createContract plutusAppId roles contract = runExceptT do
    reqId <- liftEffect genUUID
    let
      backRoles :: Map Back.TokenName Address
      backRoles = Map.fromFoldable
        $ map (over _1 toBack)
        $ (Map.toUnfoldable roles :: Array (TokenName /\ Address))

      payload = [ encodeJson reqId, encodeJson backRoles, encodeJson contract ]
    ExceptT $ PAB.invokeEndpoint plutusAppId "create" payload
    bus <- asks $ view _createBus
    pure $ Tuple reqId $ EventBus.subscribeOnce bus.emitter reqId

  applyInputs plutusAppId marloweContractId input = runExceptT do
    let
      TransactionInput { interval: TimeInterval slotStart slotEnd, inputs } =
        input
    reqId <- liftEffect genUUID
    let
      backTimeInterval :: POSIXTime /\ POSIXTime
      backTimeInterval = (slotStart) /\ (slotEnd)

      payload =
        [ encodeJson reqId
        , encodeJson marloweContractId
        , encodeJson backTimeInterval
        , encodeJson inputs
        ]
    ExceptT
      $ PAB.invokeEndpoint plutusAppId "apply-inputs-nonmerkleized" payload
    bus <- asks $ view _applyInputBus
    pure $ EventBus.subscribeOnce bus.emitter reqId

  redeem plutusAppId marloweContractId tokenName address = runExceptT do
    reqId <- liftEffect genUUID
    let
      payload =
        [ encodeJson reqId
        , encodeJson marloweContractId
        , encodeJson tokenName
        , encodeJson address
        ]
    ExceptT $ PAB.invokeEndpoint plutusAppId "redeem" payload
    bus <- asks $ view _redeemBus
    pure $ EventBus.subscribeOnce bus.emitter reqId
