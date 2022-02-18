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
  ) where

import Prologue

import API.Lenses (_cicCurrentState, _hooks, _observableState)
import AppM (AppM)
import Control.Monad.Except (lift)
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
import Data.Set as Set
import Data.These (These(..))
import Data.Traversable (sequence, traverse)
import Data.WalletId (WalletId)
import Data.WalletId as WalletId
import Effect.Aff (Aff)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Env (_endpointSemaphores)
import Halogen (HalogenM)
import Marlowe.PAB (PlutusAppId)
import MarloweContract (MarloweContract)
import Plutus.Contract.Effects (ActiveEndpoint)
import Plutus.Contract.Resumable (Request)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.PAB.Webserver as PAB
import Plutus.PAB.Webserver.Types
  ( ContractActivationArgs(..)
  , ContractInstanceClientState
  , ContractSignatureResponse
  )
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
  invokeEndpoint plutusAppId endpoint payload = do
    endpointSemaphoresAVar <- asks $ view _endpointSemaphores
    liftAff do
      endpointSemaphores <- AVar.take endpointSemaphoresAVar
      let
        appSemaphores =
          fromMaybe Map.empty $ Map.lookup plutusAppId endpointSemaphores
      endpointSemaphore <- maybe AVar.empty pure
        $ Map.lookup endpoint appSemaphores
      let
        appSemaphores' = Map.insert endpoint endpointSemaphore appSemaphores
        endpointSemaphores' =
          Map.insert plutusAppId appSemaphores' endpointSemaphores
      AVar.put endpointSemaphores' endpointSemaphoresAVar
      AVar.take endpointSemaphore
    PAB.postApiContractInstanceByContractinstanceidEndpointByEndpointname
      (encodeJson payload)
      plutusAppId
      endpoint
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
