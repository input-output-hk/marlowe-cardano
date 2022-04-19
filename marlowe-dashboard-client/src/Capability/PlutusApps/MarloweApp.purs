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
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.UUID (class MonadUUID, generateUUID)
import Control.Parallel (parOneOf)
import Data.Address (Address)
import Data.Argonaut.Encode (encodeJson)
import Data.Lens (_1, over, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Effect.Aff (Aff, Error, forkAff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Env (_applyInputBus, _createBus, _marloweAppTimeoutBlocks, _redeemBus)
import Halogen.Store.Monad (emitSelected)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as Emitter
import Halogen.Subscription.Extra (subscribeOnce)
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
    -> m
         ( AjaxResponse
             (UUID /\ Aff (Maybe (Either MarloweError MarloweParams)))
         )
  applyInputs
    :: PlutusAppId
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff (Maybe (Either MarloweError Unit))))
  -- TODO auto
  -- TODO close
  redeem
    :: PlutusAppId
    -> MarloweParams
    -> TokenName
    -> Address
    -> m (AjaxResponse (Aff (Maybe (Either MarloweError Unit))))

-- | Gives an Aff a certain number of blocks to resolve, or cancels and returns Nothing.
awaitTimeout :: forall m a. MonadEffect m => Aff a -> AppM m (Aff (Maybe a))
awaitTimeout aff = do
  blocksToWait <- asks $ view _marloweAppTimeoutBlocks
  tipSlotE <- emitSelected (selectEq _.tipSlot)
  -- Create an emitter that increments every time the tip slot changes.
  let changeCountE = Emitter.fold (const $ add 1) tipSlotE 0
  -- subscribe to the first time this count reaches or exceeds the limit.
  pure $ parOneOf
    [ subscribeOnce $ Nothing <$ Emitter.filter (_ >= blocksToWait) changeCountE
    , Just <$> aff
    ]

instance
  ( MonadError Error m
  , MonadAff m
  , MonadRec m
  , MonadAjax PAB.Api m
  , MonadUUID m
  ) =>
  MarloweApp (AppM m) where
  createContract plutusAppId roles contract = runExceptT do
    reqId <- lift generateUUID
    bus <- asks $ view _createBus
    -- Run and fork this aff now so we are already listening before we even send
    -- the request. Eliminates the risk that the response will come in before we
    -- can even subscribe to the event bus.
    awaitResponse <-
      lift $ awaitTimeout $ EventBus.subscribeOnce bus.emitter reqId
    responseFiber <- liftAff $ forkAff awaitResponse
    let
      backRoles :: Map Back.TokenName Address
      backRoles = Map.fromFoldable
        $ map (over _1 toBack)
        $ (Map.toUnfoldable roles :: Array (TokenName /\ Address))

      payload = [ encodeJson reqId, encodeJson backRoles, encodeJson contract ]
    ExceptT $ PAB.invokeEndpoint plutusAppId "create" payload
    pure $ Tuple reqId $ joinFiber responseFiber

  applyInputs plutusAppId marloweContractId input = runExceptT do
    let
      TransactionInput { interval: TimeInterval slotStart slotEnd, inputs } =
        input
    reqId <- lift generateUUID
    bus <- asks $ view _applyInputBus
    -- Run and fork this aff now so we are already listening before we even send
    -- the request. Removes the risk that the response will come in before we
    -- can even subscribe to the event bus.
    awaitResponse <-
      lift $ awaitTimeout $ EventBus.subscribeOnce bus.emitter reqId
    responseFiber <- liftAff $ forkAff awaitResponse
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
    pure $ joinFiber responseFiber

  redeem plutusAppId marloweContractId tokenName address = runExceptT do
    reqId <- lift generateUUID
    bus <- asks $ view _redeemBus
    -- Run and fork this aff now so we are already listening before we even send
    -- the request. Removes the risk that the response will come in before we
    -- can even subscribe to the event bus.
    awaitResponse <-
      lift $ awaitTimeout $ EventBus.subscribeOnce bus.emitter reqId
    responseFiber <- liftAff $ forkAff awaitResponse
    let
      payload =
        [ encodeJson reqId
        , encodeJson marloweContractId
        , encodeJson { unTokenName: tokenName }
        , encodeJson address
        ]
    ExceptT $ PAB.invokeEndpoint plutusAppId "redeem" payload
    pure $ joinFiber responseFiber
