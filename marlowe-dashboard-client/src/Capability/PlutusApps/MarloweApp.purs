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
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (asks)
import Data.Address (Address)
import Data.Argonaut.Encode (encodeJson)
import Data.Lens (_1, over, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID, genUUID)
import Effect.AVar (AVar)
import Effect.Aff (Aff, effectCanceler, launchAff_)
import Effect.Aff as Aff
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Env (_applyInputListeners, _createListeners, _redeemListeners)
import Halogen.Subscription as HS
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
    -> m (AjaxResponse (UUID /\ Aff MarloweParams))
  applyInputs
    :: PlutusAppId
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff Unit))
  -- TODO auto
  -- TODO close
  redeem
    :: PlutusAppId
    -> MarloweParams
    -> TokenName
    -> Address
    -> m (AjaxResponse (Aff Unit))

instance (MonadAff m, MonadAjax PAB.Api m) => MarloweApp (AppM m) where
  createContract plutusAppId roles contract = runExceptT do
    reqId <- liftEffect genUUID
    let
      backRoles :: Map Back.TokenName Address
      backRoles = Map.fromFoldable
        $ map (over _1 toBack)
        $ (Map.toUnfoldable roles :: Array (TokenName /\ Address))

      payload = [ encodeJson reqId, encodeJson backRoles, encodeJson contract ]
    ExceptT $ PAB.invokeEndpoint plutusAppId "create" payload
    createListenersAVar <- asks $ view _createListeners
    liftAff $ Tuple reqId <$> waitForUpdate reqId createListenersAVar

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
    applyInputListenersAVar <- asks $ view _applyInputListeners
    liftAff $ waitForUpdate reqId applyInputListenersAVar

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
    redeemListenersAVar <- asks $ view _redeemListeners
    liftAff $ waitForUpdate reqId redeemListenersAVar

waitForUpdate
  :: forall a
   . UUID
  -> AVar (Map UUID (Maybe HS.Subscription /\ HS.Listener a))
  -> Aff (Aff a)
waitForUpdate reqId listenersAVar = do
  { emitter, listener } <- liftEffect HS.create
  listeners <- AVar.take listenersAVar
  AVar.put (Map.insert reqId (Nothing /\ listener) listeners) listenersAVar
  pure $ Aff.makeAff \resolve -> do
    subscription <- HS.subscribe emitter \params -> do
      launchAff_ do
        listeners' <- AVar.take listenersAVar
        AVar.put (Map.delete reqId listeners') listenersAVar
      resolve $ pure params
    launchAff_ do
      listeners' <- AVar.take listenersAVar
      AVar.put
        (Map.insert reqId (Just subscription /\ listener) listeners')
        listenersAVar
    pure $ effectCanceler do
      launchAff_ do
        listeners' <- AVar.take listenersAVar
        AVar.put (Map.delete reqId listeners') listenersAVar
      HS.unsubscribe subscription
