module Capability.PAB
  ( class ManagePAB
  , activateContract
  , deactivateContract
  , getAllContractInstances
  , getContractDefinitions
  , getContractInstanceClientState
  , getContractInstanceCurrentState
  , getContractInstanceHooks
  , getContractInstanceObservableState
  , getWalletContractInstances
  , invokeEndpoint
  , onNewActiveEndpoints
  , subscribeToWallet
  , unsubscribeFromWallet
  , subscribeToPlutusApp
  , unsubscribeFromPlutusApp
  ) where

import Prologue

import API.Lenses (_cicCurrentState, _hooks, _observableState)
import Affjax (Error(..))
import Affjax.StatusCode (StatusCode(..))
import AppM (AppM)
import Control.Monad.Except (ExceptT(..), lift, runExcept, runExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (asks)
import Data.Align (align)
import Data.Argonaut (Json, encodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Lens (view)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Alignable (AlignableMap(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.PubKeyHash (PubKeyHash)
import Data.PubKeyHash as PKH
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.These (These(..))
import Data.Traversable (sequence, traverse)
import Data.WalletId (WalletId)
import Data.WalletId as WalletId
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Env (_endpointSemaphores, _sinks)
import Foreign.Class (decode)
import Halogen (HalogenM)
import Halogen.Subscription as HS
import Marlowe.PAB (PlutusAppId)
import MarloweContract (MarloweContract)
import Plutus.Contract.Effects (ActiveEndpoint)
import Plutus.Contract.Resumable (Request)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Webserver as PAB
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToServer(..)
  , ContractActivationArgs(..)
  , ContractInstanceClientState
  , ContractSignatureResponse
  )
import Servant.PureScript (AjaxError(..), ErrorDescription(..))
import Types (AjaxResponse)
import Wallet.Emulator.Wallet (Wallet(..))

class Monad m <= ManagePAB m where
  activateContract
    :: MarloweContract -> WalletId -> m (AjaxResponse PlutusAppId)
  deactivateContract :: PlutusAppId -> m (AjaxResponse Unit)
  getContractInstanceClientState
    :: PlutusAppId
    -> m (AjaxResponse (ContractInstanceClientState MarloweContract))
  getContractInstanceCurrentState
    :: PlutusAppId -> m (AjaxResponse (PartiallyDecodedResponse ActiveEndpoint))
  getContractInstanceObservableState :: PlutusAppId -> m (AjaxResponse Json)
  getContractInstanceHooks
    :: PlutusAppId -> m (AjaxResponse (Array (Request ActiveEndpoint)))
  invokeEndpoint
    :: forall d
     . EncodeJson d
    => PlutusAppId
    -> String
    -> d
    -> m (AjaxResponse Unit)
  getWalletContractInstances
    :: WalletId
    -> m (AjaxResponse (Array (ContractInstanceClientState MarloweContract)))
  getAllContractInstances :: m
    (AjaxResponse (Array (ContractInstanceClientState MarloweContract)))
  getContractDefinitions :: m
    (AjaxResponse (Array (ContractSignatureResponse MarloweContract)))
  onNewActiveEndpoints :: PlutusAppId -> Array ActiveEndpoint -> m Unit
  subscribeToPlutusApp :: PlutusAppId -> m Unit
  subscribeToWallet :: WalletId -> m Unit
  unsubscribeFromPlutusApp :: PlutusAppId -> m Unit
  unsubscribeFromWallet :: WalletId -> m Unit

instance ManagePAB AppM where
  activateContract contractActivationId wallet =
    PAB.postApiContractActivate
      $ ContractActivationArgs
          { caID: contractActivationId
          , caWallet: Just $ Wallet { getWalletId: WalletId.toString wallet }
          }

  deactivateContract =
    PAB.putApiContractInstanceByContractinstanceidStop

  getContractInstanceClientState =
    PAB.getApiContractInstanceByContractinstanceidStatus

  getContractInstanceCurrentState plutusAppId = do
    clientState <- getContractInstanceClientState plutusAppId
    pure $ map (view _cicCurrentState) clientState

  getContractInstanceObservableState plutusAppId = do
    currentState <- getContractInstanceCurrentState plutusAppId
    pure $ map (view _observableState) currentState

  getContractInstanceHooks plutusAppId = do
    currentState <- getContractInstanceCurrentState plutusAppId
    pure $ map (view _hooks) currentState

  invokeEndpoint appId endpoint payload = runExceptT do
    result <- ExceptT tryInvokeEndpoint
    ExceptT case result of
      Nothing -> waitForEndpointAndRetry
      Just a -> pure $ Right a
    where
    -- Tries to call the endpoint. If it's unavailable, returns a successful
    -- result with Nothing.
    tryInvokeEndpoint = do
      ajaxResult <-
        PAB.postApiContractInstanceByContractinstanceidEndpointByEndpointname
          (encodeJson payload)
          appId
          endpoint
      pure $ case ajaxResult of
        Left
          ( AjaxError
              -- This is a ConnectingError, not an UnexpectedHTTPStatus becase to
              -- create an UnexpectedHTTPStatus, servant-support needs to get a
              -- `Response Json` back from affjax. However, the body is in fact a
              -- String produced by calling Show, not JSON. Affjax fails to parse
              -- this as JSON and throws an error instead. So, we need to dig
              -- into the Error returned from Affjax to get the underlying
              -- `Response Foreign` and inspect that instead.
              { description: ConnectingError
                  (ResponseBodyError _ { status: StatusCode 500, body })
              }
          ) ->
          case runExcept $ decode body of
            Right str
              | contains (Pattern "EndpointNotAvailable") str -> Right Nothing
            _ -> Just <$> ajaxResult
        _ -> Just <$> ajaxResult

    waitForEndpointAndRetry = do
      -- Get the AVar for the global semaphore lookup
      applicationsAvar <- asks $ view _endpointSemaphores
      -- Get the AVar that corresponds to this specific endpoint
      endpointSem <- liftAff $ getOrCreateEndpoint applicationsAvar
      -- Wait until it is available
      liftAff $ AVar.take endpointSem
      -- Try again
      invokeEndpoint appId endpoint payload

    getOrCreateEndpoint applicationsAvar = do
      applications <- AVar.take applicationsAvar
      -- Get the mapping of AVars for this plutusAppId, or create a new one if
      -- it doesn't exist.
      let endpoints = fromMaybe Map.empty $ Map.lookup appId applications
      -- Get the AVar this endpoint, or create a new one if it doesn't exist.
      endpointSem <- maybe AVar.empty pure $ Map.lookup endpoint endpoints
      -- Insert the newly created AVar into the AVar Map for this application
      let endpoints' = Map.insert endpoint endpointSem endpoints
      -- Insert the newly updates AVar mapping for this app into the global
      -- map.
      let applications' = Map.insert appId endpoints' applications
      -- Put it back into the AVar.
      AVar.put applications' applicationsAvar
      -- Return the AVar for the endpoint.
      pure endpointSem

  getWalletContractInstances wallet =
    PAB.getApiContractInstancesWalletByWalletid (WalletId.toString wallet)
      Nothing

  getAllContractInstances = PAB.getApiContractInstances Nothing

  getContractDefinitions = PAB.getApiContractDefinitions

  onNewActiveEndpoints appId endpoints = do
    let
      endpointMap :: Map String Unit
      endpointMap = Set.toMap $ Set.fromFoldable $ map
        (_.getEndpointDescription <<< unwrap <<< _.aeDescription <<< unwrap)
        endpoints
    endpointSemaphoresAVar <- asks $ view _endpointSemaphores
    liftAff do
      endpointSemaphores <- AVar.take endpointSemaphoresAVar
      appSemaphores' <- case Map.lookup appId endpointSemaphores of
        Nothing -> traverse (const $ AVar.new unit) endpointMap
        Just appSemaphores -> sequence $ unwrap $ align updateSemaphore
          (AlignableMap appSemaphores)
          (AlignableMap endpointMap)
      AVar.put
        (Map.insert appId appSemaphores' endpointSemaphores)
        endpointSemaphoresAVar
    where
    -- endpoint was in semaphores, but not in new available endpoints. Lock it
    -- (without blocking).
    updateSemaphore :: These (AVar Unit) Unit -> Aff (AVar Unit)
    updateSemaphore (This semaphore) = AVar.tryTake semaphore $> semaphore
    -- endpoint was in new available endpoints, but not in semaphores. Create a
    -- new, unlocked semaphore for it.
    updateSemaphore (That _) = AVar.new unit
    -- endpoint was in both available endpoints, and semaphores. Unlock it.
    updateSemaphore (Both semaphore _) = AVar.put unit semaphore $> semaphore
  subscribeToPlutusApp = Left >>> Subscribe >>> sendWsMessage
  subscribeToWallet =
    sendWsMessage <<< Subscribe <<< Right <<< invalidWalletIdToPubKeyHash
  unsubscribeFromPlutusApp = Left >>> Unsubscribe >>> sendWsMessage
  unsubscribeFromWallet =
    sendWsMessage <<< Unsubscribe <<< Right <<< invalidWalletIdToPubKeyHash

sendWsMessage :: CombinedWSStreamToServer -> AppM Unit
sendWsMessage msg = do
  { pabWebsocket } <- asks $ view _sinks
  liftEffect $ HS.notify pabWebsocket msg

-- | DO NOT USE! This is incorrect. The WS message type _should_ require a
-- | wallet ID, not a pub key hash. This is the cost of using strings for types,
-- | even if wrapped in newtypes!
invalidWalletIdToPubKeyHash :: WalletId -> PubKeyHash
invalidWalletIdToPubKeyHash = PKH.fromString <<< WalletId.toString

instance ManagePAB m => ManagePAB (HalogenM state action slots msg m) where
  activateContract contractActivationId wallet = lift $ activateContract
    contractActivationId
    wallet
  deactivateContract = lift <<< deactivateContract
  getContractInstanceClientState = lift <<< getContractInstanceClientState
  getContractInstanceCurrentState = lift <<< getContractInstanceCurrentState
  getContractInstanceObservableState = lift <<<
    getContractInstanceObservableState
  getContractInstanceHooks = lift <<< getContractInstanceHooks
  invokeEndpoint plutusAppId endpointDescription payload = lift $ invokeEndpoint
    plutusAppId
    endpointDescription
    payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  getAllContractInstances = lift getAllContractInstances
  getContractDefinitions = lift getContractDefinitions
  onNewActiveEndpoints appId = lift <<< onNewActiveEndpoints appId
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet

instance ManagePAB m => ManagePAB (MaybeT m) where
  activateContract contractActivationId wallet = lift $ activateContract
    contractActivationId
    wallet
  deactivateContract = lift <<< deactivateContract
  getContractInstanceClientState = lift <<< getContractInstanceClientState
  getContractInstanceCurrentState = lift <<< getContractInstanceCurrentState
  getContractInstanceObservableState = lift <<<
    getContractInstanceObservableState
  getContractInstanceHooks = lift <<< getContractInstanceHooks
  invokeEndpoint plutusAppId endpointDescription payload = lift $ invokeEndpoint
    plutusAppId
    endpointDescription
    payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  getAllContractInstances = lift getAllContractInstances
  getContractDefinitions = lift getContractDefinitions
  onNewActiveEndpoints appId = lift <<< onNewActiveEndpoints appId
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet
