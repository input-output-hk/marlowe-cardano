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
  , onNewObservableState
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Bridge (toBack)
import Capability.PAB (class ManagePAB)
import Capability.PAB (invokeEndpoint) as PAB
import Capability.PlutusApps.MarloweApp.Types
  ( Endpoints
  , MarloweEndpointResponse
  )
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Trans.Class (lift)
import Data.Address (Address)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (findMap, take, (:))
import Data.Foldable (elem)
import Data.Lens (_1, over, to, toArrayOf, traversed, view)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.PubKeyHash (PubKeyHash)
import Data.Traversable (for)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID, genUUID)
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Env (Env, _marloweEndpoints, _pendingRequests)
import Language.Marlowe.Client (EndpointResponse(..))
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
    -> PubKeyHash
    -> m (AjaxResponse Unit)

instance marloweAppM :: MarloweApp AppM where
  createContract plutusAppId roles contract = do
    reqId <- liftEffect genUUID
    let
      backRoles :: Map Back.TokenName Address
      backRoles = Map.fromFoldable
        $ map (over _1 toBack)
        $ (Map.toUnfoldable roles :: Array (TokenName /\ Address))

      payload = [ encodeJson reqId, encodeJson backRoles, encodeJson contract ]
    map (const reqId)
      <$> invokeEndpointAvar plutusAppId reqId "create" _.create payload
  applyInputs
    plutusAppId
    marloweContractId
    (TransactionInput { interval: TimeInterval slotStart slotEnd, inputs }) = do
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
    invokeEndpointAvar
      plutusAppId
      reqId
      "apply-inputs-nonmerkleized"
      _.applyInputs
      payload
  redeem plutusAppId marloweContractId tokenName pubKeyHash = do
    reqId <- liftEffect genUUID
    let
      payload =
        [ encodeJson reqId
        , encodeJson marloweContractId
        , encodeJson tokenName
        , encodeJson pubKeyHash
        ]
    invokeEndpointAvar plutusAppId reqId "redeem" _.redeem payload

-- This is the amount of requests we store in the request queue
maxRequests :: Int
maxRequests = 15

invokeEndpointAvar
  :: forall payload m
   . EncodeJson payload
  => MonadAff m
  => ManagePAB m
  => MonadAsk Env m
  => PlutusAppId
  -> UUID
  -> String
  -> (Endpoints -> (AVar Unit))
  -> payload
  -> m (AjaxResponse Unit)
invokeEndpointAvar plutusAppId reqId endpointName getEndpoint payload =
  runExceptT do
    -- There are three AVars involved in this operation:
    --   * The endpoint help us avoid making multiple requests to the same endpoint if its not
    --     available.
    --   * The global pendingRequests help us manage the request queue concurrently.
    --   * For each request in the queue, we store one mutex that represents the response.
    endpoint <- lift $ asks $ view (_marloweEndpoints <<< to getEndpoint)
    pendingRequests <- lift $ asks $ view _pendingRequests
    -- First we need to see if the endpoint is available to make a request
    -- TODO: we could later add a forkAff with a timer that unlocks this timer if we
    --       dont get a response
    -- TODO: We could change this take for a read and listen for a 500 error with EndpointUnavailabe
    --       to retry and take it (instead of preemptively taking it).
    liftAff $ AVar.take endpoint
    result <- ExceptT $ PAB.invokeEndpoint plutusAppId endpointName payload
    -- After making the request successfully, we lock on the global request array so we can add
    -- a new request id with its corresponding mutex.
    -- TODO: It would be nice if the invokeEndpoint returns a requestId instead of having to generate
    --       one from the FE.
    liftAff do
      requests <- AVar.take pendingRequests
      newRequest <- AVar.empty
      let
        -- We add new requests to the begining of the array and remove them from the end.
        newRequests = take maxRequests $ (reqId /\ newRequest) : requests
      AVar.put newRequests pendingRequests
    pure result

-- When a MarloweApp emits a new observable state, it comes with the information of which
-- request id originated the change. We use this function to update the request queue and
-- free any thread that might be waiting for the result of an action.
-- The return type is a Maybe of the same input, if the value is Nothing, then we couldn't
-- find a request that triggered this response. This can happen for example when reloading
-- the webpage, or if we have two browsers with the same wallet.
onNewObservableState
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MarloweEndpointResponse
  -> m (Maybe MarloweEndpointResponse)
onNewObservableState lastResult = case lastResult of
  EndpointSuccess reqId _ -> onNewObservableState' reqId
  EndpointException reqId _ _ -> onNewObservableState' reqId
  where
  onNewObservableState' reqId = do
    pendingRequests <- asks $ view _pendingRequests
    -- This read is blocking but does not take the mutex.
    requests <- liftAff $ AVar.read pendingRequests
    for (findReqId reqId requests) \reqMutex -> do
      liftAff $ AVar.put lastResult reqMutex
      pure lastResult

findReqId
  :: UUID
  -> Array (UUID /\ AVar MarloweEndpointResponse)
  -> Maybe (AVar MarloweEndpointResponse)
findReqId reqId = findMap
  (\(reqId' /\ reqMutex) -> if reqId == reqId' then Just reqMutex else Nothing)

-- Plutus contracts have endpoints that can be available or not. We get notified by the
-- websocket message NewActiveEndpoints when the status change, and we use this function
-- to update some mutex we use to restrict access to unavailable methods.
onNewActiveEndpoints
  :: forall m. MonadAff m => MonadAsk Env m => Array ActiveEndpoint -> m Unit
onNewActiveEndpoints endpoints = do
  let
    endpointsName :: Array String
    endpointsName =
      toArrayOf
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
      if (elem name endpointsName) then
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
