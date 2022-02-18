-- This capability exposes the API for the Plutus Aplication called `MarloweApp`, which
-- has it's endpoints defined as the MarloweSchema in Language.Marlowe.Client module.
-- There is one `MarloweApp` per wallet, and it serves as a control app for all
-- the Marlowe contracts that the wallet has a role token.
module Capability.PlutusApps.MarloweApp
  ( class MarloweApp
  , createContract
  , applyInputs
  , maxRequests
  , onNewActiveEndpoints
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Bridge (toBack)
import Capability.PAB (class ManagePAB)
import Capability.PAB (invokeEndpoint) as PAB
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Address (Address)
import Data.Argonaut (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (take, (:))
import Data.Lens (_1, over, to, traversed, view)
import Data.Lens.Extra (toSetOf)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID, genUUID)
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Env (Env, _marloweEndpoints, _pendingResults)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( Contract
  , MarloweParams
  , TimeInterval(..)
  , TokenName
  , TransactionInput(..)
  )
import Plutus.Contract.Effects (ActiveEndpoint, _ActiveEndpoint)
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (TokenName) as Back
import Type.Proxy (Proxy(..))
import Types (AjaxResponse)
import Wallet.Types (_EndpointDescription)

class MarloweApp m where
  createContract
    :: PlutusAppId
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse UUID)
  applyInputs
    :: PlutusAppId -> MarloweParams -> TransactionInput -> m (AjaxResponse Unit)
  -- TODO auto
  -- TODO close
  redeem
    :: PlutusAppId
    -> MarloweParams
    -> TokenName
    -> Address
    -> m (AjaxResponse Unit)

instance marloweAppM :: MarloweApp AppM where
  createContract plutusAppId roles contract = do
    endpoints <- asks $ view _marloweEndpoints
    let
      backRoles :: Map Back.TokenName Address
      backRoles = Map.fromFoldable
        $ map (over _1 toBack)
        $ (Map.toUnfoldable roles :: Array (TokenName /\ Address))

      payload = [ encodeJson backRoles, encodeJson contract ]
    enqueueResultHandler =<< invokeEndpointAvar
      endpoints.create
      plutusAppId
      "create"
      payload

  applyInputs
    plutusAppId
    marloweContractId
    (TransactionInput { interval: TimeInterval slotStart slotEnd, inputs }) = do
    endpoints <- asks $ view _marloweEndpoints
    let
      backTimeInterval :: POSIXTime /\ POSIXTime
      backTimeInterval = (slotStart) /\ (slotEnd)

      payload =
        [ encodeJson marloweContractId
        , encodeJson backTimeInterval
        , encodeJson inputs
        ]
    map void <$> enqueueResultHandler =<< invokeEndpointAvar
      endpoints.applyInputs
      plutusAppId
      "apply-inputs-nonmerkleized"
      payload

  redeem plutusAppId marloweContractId tokenName address = do
    endpoints <- asks $ view _marloweEndpoints
    let
      payload =
        [ encodeJson marloweContractId
        , encodeJson tokenName
        , encodeJson address
        ]
    map void <$> enqueueResultHandler =<< invokeEndpointAvar
      endpoints.redeem
      plutusAppId
      "redeem"
      payload

-- This is the amount of requests we store in the request queue
maxRequests :: Int
maxRequests = 15

enqueueResultHandler :: AjaxResponse UUID -> AppM (AjaxResponse UUID)
enqueueResultHandler (Left e) = pure $ Left e
enqueueResultHandler (Right reqId) = do
  pendingResults <- asks $ view _pendingResults
  liftAff do
    pendingResults' <- AVar.take pendingResults
    pendingResult <- AVar.empty
    -- We add new pending results to the begining of the array and remove them from the end.
    AVar.put
      (take maxRequests $ (reqId /\ pendingResult) : pendingResults')
      pendingResults
  pure $ Right reqId

invokeEndpointAvar
  :: forall m
   . MonadAff m
  => ManagePAB m
  => AVar Unit
  -> PlutusAppId
  -> String
  -> Array Json
  -> m (AjaxResponse UUID)
invokeEndpointAvar endpointAVar plutusAppId endpointName payload = runExceptT do
  -- Check if the endpoint is available to make a request
  -- TODO: we could later add a forkAff with a timer that unlocks this timer if we
  --       dont get a response
  -- TODO: We could change this take for a read and listen for a 500 error with EndpointUnavailabe
  --       to retry and take it (instead of preemptively taking it).
  reqId <- liftEffect genUUID
  liftAff $ AVar.take endpointAVar
  ExceptT
    $ PAB.invokeEndpoint plutusAppId endpointName (encodeJson reqId : payload)
  pure reqId

-- Plutus contracts have endpoints that can be available or not. We get notified by the
-- websocket message NewActiveEndpoints when the status change, and we use this function
-- to update some mutex we use to restrict access to unavailable methods.
onNewActiveEndpoints
  :: forall m. MonadAff m => MonadAsk Env m => Array ActiveEndpoint -> m Unit
onNewActiveEndpoints endpoints = do
  let
    endpointNames :: Set String
    endpointNames =
      toSetOf
        ( traversed
            <<< _ActiveEndpoint
            <<< prop (Proxy :: _ "aeDescription")
            <<< _EndpointDescription
            <<< prop (Proxy :: _ "getEndpointDescription")
        )
        endpoints

    -- For each endpoint:
    updateEndpoint name getter = do
      mutex <- asks $ view (_marloweEndpoints <<< to getter)
      -- We check if it's available or not
      if Set.member name endpointNames then
        -- If it's available we put a unit in the mutex, to allow
        -- users to call the endpoint. If the mutex already has a unit,
        -- `tryPut` will return false but wont block the thread.
        void $ liftAff $ AVar.tryPut unit mutex
      else
        -- If it's not available we remove a unit from the mutex to make
        -- callers to wait until we put a unit. If the mutex was already
        -- empty, tryTake will return Nothing but wont block the thread.
        void $ liftAff $ AVar.tryTake mutex
  updateEndpoint "redeem" _.redeem
  updateEndpoint "create" _.create
  updateEndpoint "apply-inputs-nonmerkleized" _.applyInputs
