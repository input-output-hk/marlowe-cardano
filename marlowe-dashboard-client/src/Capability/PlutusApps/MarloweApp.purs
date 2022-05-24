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
import Capability.PAB (invokeEndpoint) as PAB
import Control.Concurrent.EventBus (EventBus)
import Control.Concurrent.EventBus as EventBus
import Control.Monad.Cont.Trans (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Fork.Class (class MonadBracket)
import Control.Monad.Reader (asks)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.UUID (class MonadUUID, generateUUID)
import Data.Address (Address)
import Data.Argonaut (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array ((:))
import Data.Bifunctor (lmap)
import Data.Lens (Lens', view)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Data.WalletId (WalletId)
import Effect.Aff (Aff, Error, forkAff, joinFiber)
import Effect.Aff.Class (class MonadAff, liftAff)
import Env (Env, _applyInputBus, _createBus, _redeemBus)
import Language.Marlowe.Client (MarloweError)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Server as Marlowe
import Marlowe.Semantics
  ( Contract
  , MarloweParams
  , TimeInterval(..)
  , TokenName
  , TransactionInput(..)
  )
import Plutus.PAB.Webserver (Api) as PAB
import Servant.PureScript (class MonadAjax)
import Store.RoleTokens (Payout)
import Types (AjaxResponse)

class MarloweApp m where
  createContract
    :: WalletId
    -> PlutusAppId
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse (UUID /\ Aff (Either MarloweError MarloweParams)))
  applyInputs
    :: WalletId
    -> PlutusAppId
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff (Either MarloweError Unit)))
  -- TODO auto
  -- TODO close
  redeem
    :: WalletId
    -> PlutusAppId
    -> Payout
    -> Address
    -> m (AjaxResponse (Aff (Either MarloweError Unit)))

instance
  ( MonadBracket Error f m
  , MonadAff m
  , MonadRec m
  , MonadAjax PAB.Api m
  , MonadAjax Marlowe.Api m
  , MonadUUID m
  ) =>
  MarloweApp (AppM m) where
  createContract walletId marloweAppId roles contract =
    invokeMarloweAppEndpoint walletId _createBus marloweAppId "create"
      [ encodeJson
          $ Map.fromFoldable
          $ map (lmap { unTokenName: _ })
          $ (Map.toUnfoldable roles :: Array _)
      , encodeJson contract
      ]

  applyInputs walletId marloweAppId marloweParams input = do
    let
      TransactionInput { interval, inputs } = input
      TimeInterval invalidBefore invalidHereafter = interval
      endpoint = "apply-inputs-nonmerkleized"
    map snd <$> invokeMarloweAppEndpoint walletId _applyInputBus marloweAppId
      endpoint
      [ encodeJson marloweParams
      , encodeJson $ invalidBefore /\ invalidHereafter
      , encodeJson inputs
      ]

  redeem walletId marloweAppId { marloweParams, tokenName } address =
    map snd <$> invokeMarloweAppEndpoint walletId _redeemBus marloweAppId
      "redeem"
      [ encodeJson marloweParams
      , encodeJson { unTokenName: tokenName }
      , encodeJson address
      ]

invokeMarloweAppEndpoint
  :: forall m f a
   . MonadUUID m
  => MonadAff m
  => MonadRec m
  => MonadAjax Marlowe.Api m
  => MonadAjax PAB.Api m
  => MonadBracket Error f m
  => WalletId
  -> Lens' Env (EventBus UUID a)
  -> PlutusAppId
  -> String
  -> Array Json
  -> AppM m (AjaxResponse (Tuple UUID (Aff a)))
invokeMarloweAppEndpoint walletId busLens marloweAppId endpoint payload =
  runExceptT do
    reqId <- lift generateUUID
    bus <- asks $ view busLens
    -- Run and fork this aff now so we are already listening before we even send
    -- the request. Eliminates the risk that the response will come in before we
    -- can even subscribe to the event bus.
    responseFiber <-
      liftAff $ forkAff $ EventBus.subscribeOnce bus.emitter reqId
    ExceptT $ PAB.invokeEndpoint walletId marloweAppId endpoint
      $ encodeJson reqId : payload
    pure $ Tuple reqId $ joinFiber responseFiber
