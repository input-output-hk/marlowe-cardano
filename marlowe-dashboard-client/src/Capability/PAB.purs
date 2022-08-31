module Capability.PAB
  ( class ManagePAB
  , activateContract
  , stopContract
  , getWalletContractInstances
  , invokeEndpoint
  , onNewActiveEndpoints
  , subscribeToPlutusApp
  , unsubscribeFromPlutusApp
  ) where

import Prologue

import Affjax (defaultRequest)
import Affjax as Affjax
import Affjax.ResponseFormat as ResponseFormat
import Affjax.StatusCode (StatusCode(..))
import AppM (AppM)
import Control.Concurrent.AVarMap as AVarMap
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..), lift, runExcept, runExceptT)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Reader (ReaderT, asks)
import Control.Monad.Rec.Class (class MonadRec, untilJust)
import Control.Parallel (parOneOf)
import Data.Align (align)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Data.Filterable (filter)
import Data.Lens (view)
import Data.Map (Map)
import Data.Map.Alignable (AlignableMap(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.String (Pattern(..), contains)
import Data.These (theseLeft)
import Data.Time.Duration (Minutes(..), fromDuration)
import Data.Traversable (sequence)
import Data.UUID.Argonaut as UUID
import Data.WalletId (WalletId)
import Data.WalletId as WalletId
import Effect.Aff (Error, delay, error)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Env (_endpointAVarMap, _pabAVar, _sinks)
import Foreign.Class (decode)
import Halogen (HalogenM)
import Halogen.Subscription as HS
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Server as Marlowe
import MarloweContract (MarloweContract)
import Plutus.Contract.Effects (ActiveEndpoint)
import Plutus.PAB.Webserver as PAB
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToServer(..)
  , ContractActivationArgs(..)
  , ContractInstanceClientState
  )
import Servant.PureScript (class MonadAjax, AjaxError(..), ErrorDescription(..))
import Types (AjaxResponse)
import Wallet.Emulator.Wallet (Wallet(..))

class Monad m <= ManagePAB m where
  activateContract
    :: MarloweContract -> WalletId -> m (AjaxResponse PlutusAppId)
  stopContract :: PlutusAppId -> m (AjaxResponse Unit)
  invokeEndpoint
    :: forall d
     . EncodeJson d
    => WalletId
    -> PlutusAppId
    -> String
    -> d
    -> m (AjaxResponse Unit)
  getWalletContractInstances
    :: WalletId
    -> m (AjaxResponse (Array (ContractInstanceClientState MarloweContract)))
  onNewActiveEndpoints :: PlutusAppId -> Array ActiveEndpoint -> m Unit
  subscribeToPlutusApp :: PlutusAppId -> m Unit
  unsubscribeFromPlutusApp :: PlutusAppId -> m Unit

-- This is a hack to protect the PAB from its own poor handling of wallet
-- backend failures. Some of our contracts perform `ownPublicKey` requests
-- which perform wallet backend API calls. If these calls fail, the contract
-- instance's thread dies and the instance becomes an unkillable zombie which
-- can only be cleared by restarting the PAB and deleting the contract instance
-- from the database.
--
-- The most common reason why these calls fail is because we provide a wallet
-- ID that the wallet backend doesn't have in its database (this happens often
-- with the in memory wallet store). The safeguard against this, we call our
-- own `GET total-funds` endpoint which also calls the wallet backend and fails
-- if the wallet backend does. If this call fails, we don't even try sending
-- anything to the PAB.
--
-- The common case of this is starting a wallet companion before we've polled
-- the wallet backend (e.g. when refreshing and using the locally saved wallet
-- information to connect). This results in an apparently "active" wallet
-- companion instance which is nonetheless dead, and never sends any updates.
assertWalletStatus
  :: forall m
   . MonadAjax Marlowe.Api m
  => MonadError Error m
  => MonadEffect m
  => WalletId
  -> AppM m (AjaxResponse Unit)
assertWalletStatus wallet = do
  response <- Marlowe.getApiWalletV1ByWalletidTotalfunds wallet
  pure $ void response

instance
  ( MonadRec m
  , MonadAff m
  , MonadBracket Error f m
  , MonadAjax PAB.Api m
  , MonadAjax Marlowe.Api m
  ) =>
  ManagePAB (AppM m) where
  stopContract instanceId = do
    -- Ugly hack to try and prevent multiple simultaneous requests to the PAB.
    -- This can result in the SQLite database being locked and an error being
    -- thrown.
    pabAvar <- asks $ view _pabAVar
    bracket
      (liftAff $ AVar.take pabAvar)
      (\_ -> liftAff <<< flip AVar.put pabAvar)
      \_ -> PAB.putApiContractInstanceByContractinstanceidStop instanceId
  activateContract contractActivationId wallet = do
    -- Ugly hack to try and prevent multiple simultaneous requests to the PAB.
    -- This can result in the SQLite database being locked and an error being
    -- thrown.
    pabAvar <- asks $ view _pabAVar
    bracket
      (liftAff $ AVar.take pabAvar)
      (\_ -> liftAff <<< flip AVar.put pabAvar)
      \_ -> runExceptT do
        -- Ugly hack to prevent the PAB getting called with a wallet ID the wallet
        -- backend doesn't know about. This can cause zombie app instances to be
        -- spawned.
        ExceptT $ assertWalletStatus wallet

        ExceptT $ PAB.postApiContractActivate
          $ ContractActivationArgs
              { caID: contractActivationId
              , caWallet: Just $ Wallet
                  { prettyWalletName: Nothing
                  , getWalletId: WalletId.toString wallet
                  }
              }

  invokeEndpoint wallet appId endpoint payload =
    runExceptT $ untilJust $ runMaybeT do
      endpointAvarMap <- asks $ view _endpointAVarMap
      -- Try to take the endpoint's availability AVar with a 5 minute timeout
      lift $ ExceptT $ liftAff $ parOneOf
        [ Right <$> AVarMap.take (Tuple appId endpoint) endpointAvarMap
        , Left timeoutError <$ delay (fromDuration $ Minutes 5.0)
        ]
      -- Ugly hack to prevent the PAB getting called with a wallet ID the wallet
      -- backend doesn't know about. This can cause zombie app instances to be
      -- spawned.
      lift $ ExceptT $ assertWalletStatus wallet

      ajaxResult <- lift $ lift $
        PAB.postApiContractInstanceByContractinstanceidEndpointByEndpointname
          (encodeJson payload)
          appId
          endpoint
      MaybeT
        $ ExceptT
        $ pure
        $ sequence
        $ filter (not <<< isEndpointNotAvailableFailure)
        $ Just ajaxResult
    where
    timeoutError = AjaxError
      { request: defaultRequest { responseFormat = ResponseFormat.json }
      , response: Nothing
      , description: ConnectingError $ Affjax.XHROtherError $ error $
          "Timed out waiting for endpoint to be released. AppId: "
            <> UUID.toString (unwrap appId)
            <> ", Endpoint: "
            <> endpoint
      }

    isEndpointNotAvailableFailure = case _ of
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
                (Affjax.ResponseBodyError _ { status: StatusCode 500, body })
            }
        ) ->
        case runExcept $ decode body of
          Right str -> contains (Pattern "EndpointNotAvailable") str
          _ -> false
      _ -> false

  getWalletContractInstances wallet = runExceptT do
    -- Ugly hack to prevent the PAB getting called with a wallet ID the wallet
    -- backend doesn't know about. This can cause zombie app instances to be
    -- spawned.
    ExceptT $ assertWalletStatus wallet

    ExceptT $ PAB.getApiContractInstancesWalletByWalletid
      (WalletId.toString wallet)
      Nothing

  onNewActiveEndpoints appId endpoints = do
    let
      endpointMap :: Map (Tuple PlutusAppId String) Unit
      endpointMap = Set.toMap $ Set.fromFoldable $ map
        ( Tuple appId
            <<< _.getEndpointDescription
            <<< unwrap
            <<< _.aeDescription
            <<< unwrap
        )
        endpoints
    endpointAvarMap <- asks $ view _endpointAVarMap
    keys <- Set.filter (eq appId <<< fst) <$> AVarMap.keys endpointAvarMap
    let
      appMask :: Map (Tuple PlutusAppId String) (Maybe Unit)
      appMask = unwrap $ align
        theseLeft
        (AlignableMap endpointMap)
        (AlignableMap $ Set.toMap keys)
    AVarMap.mask appMask endpointAvarMap
  subscribeToPlutusApp = Left >>> Subscribe >>> sendWsMessage
  unsubscribeFromPlutusApp = Left >>> Unsubscribe >>> sendWsMessage

sendWsMessage
  :: forall m. MonadEffect m => CombinedWSStreamToServer -> AppM m Unit
sendWsMessage msg = do
  { pabWebsocket } <- asks $ view _sinks
  liftEffect $ HS.notify pabWebsocket msg

instance ManagePAB m => ManagePAB (HalogenM state action slots msg m) where
  stopContract = lift <<< stopContract
  activateContract contractActivationId wallet = lift $ activateContract
    contractActivationId
    wallet
  invokeEndpoint wallet plutusAppId endpointDescription payload = lift $
    invokeEndpoint wallet plutusAppId endpointDescription payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  onNewActiveEndpoints appId = lift <<< onNewActiveEndpoints appId
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp

instance ManagePAB m => ManagePAB (MaybeT m) where
  stopContract = lift <<< stopContract
  activateContract contractActivationId wallet = lift $ activateContract
    contractActivationId
    wallet
  invokeEndpoint wallet plutusAppId endpointDescription payload = lift $
    invokeEndpoint wallet plutusAppId endpointDescription payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  onNewActiveEndpoints appId = lift <<< onNewActiveEndpoints appId
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp

instance ManagePAB m => ManagePAB (ReaderT r m) where
  stopContract = lift <<< stopContract
  activateContract contractActivationId wallet = lift $ activateContract
    contractActivationId
    wallet
  invokeEndpoint wallet plutusAppId endpointDescription payload = lift $
    invokeEndpoint wallet plutusAppId endpointDescription payload
  getWalletContractInstances = lift <<< getWalletContractInstances
  onNewActiveEndpoints appId = lift <<< onNewActiveEndpoints appId
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
