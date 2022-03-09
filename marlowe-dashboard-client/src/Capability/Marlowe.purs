module Capability.Marlowe
  ( class ManageMarlowe
  , followContract
  , initializeContract
  , applyTransactionInput
  , redeem
  , getRoleContracts
  , subscribeToWallet
  , unsubscribeFromWallet
  , subscribeToPlutusApp
  , unsubscribeFromPlutusApp
  ) where

import Prologue

import AppM (AppM)
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.PAB (class ManagePAB)
import Capability.PAB
  ( activateContract
  , getContractInstanceObservableState
  , invokeEndpoint
  ) as PAB
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Wallet (class ManageWallet)
import Component.ContractSetup.Types (ContractParams)
import Component.Template.State
  ( InstantiateContractErrorRow
  , instantiateExtendedContract
  )
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (asks)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant)
import Data.Either (note)
import Data.Lens (view)
import Data.Map (Map)
import Data.Maybe (maybe')
import Data.NewContract (NewContract(..))
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _address
  , _companionAppId
  , _marloweAppId
  , _walletId
  )
import Data.PubKeyHash (PubKeyHash)
import Data.PubKeyHash as PKH
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Generic (class Constructors, mkConstructors')
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Env (_sinks)
import Halogen (HalogenM)
import Halogen.Store.Monad (getStore, updateStore)
import Halogen.Subscription as HS
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Client (getContract)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( MarloweData
  , MarloweParams
  , TokenName
  , TransactionInput
  )
import MarloweContract (MarloweContract(..))
import Plutus.PAB.Webserver.Types (CombinedWSStreamToServer(..))
import Store as Store
import Store.Contracts (getFollowerContract)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Types
  ( AjaxResponse
  , DecodedAjaxResponse
  , JsonAjaxErrorRow
  , JsonDecodeErrorRow
  , MetadataNotFoundError(..)
  , MetadataNotFoundErrorRow
  )

type FollowContractError = Variant
  (JsonAjaxErrorRow + MetadataNotFoundErrorRow + JsonDecodeErrorRow + ())

followContractError :: forall c. Constructors FollowContractError c => c
followContractError = mkConstructors' (Proxy :: Proxy FollowContractError)

type InitializeContractError = Variant
  (JsonAjaxErrorRow + InstantiateContractErrorRow + ())

initializeContractError
  :: forall c. Constructors InitializeContractError c => c
initializeContractError = mkConstructors'
  (Proxy :: Proxy InitializeContractError)

-- The `ManageMarlowe` class provides a window on the `ManagePAB` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ( ManagePAB m
  , ManageMarloweStorage m
  , ManageWallet m
  ) <=
  ManageMarlowe m where
  followContract
    :: PABConnectedWallet
    -> MarloweParams
    -> m (Either FollowContractError (PlutusAppId /\ ContractHistory))
  initializeContract
    :: Instant
    -> ContractTemplate
    -> ContractParams
    -> PABConnectedWallet
    -> m (Either InitializeContractError (NewContract /\ Aff MarloweParams))
  applyTransactionInput
    :: PABConnectedWallet
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff Unit))
  redeem
    :: PABConnectedWallet
    -> MarloweParams
    -> TokenName
    -> m (AjaxResponse (Aff Unit))
  getRoleContracts
    :: PABConnectedWallet
    -> m (DecodedAjaxResponse (Map MarloweParams MarloweData))

  -- TODO: SCP-3543 Remove this endpoint from here and Encapsultate subscribe/unsubscribe logic into
  --       a separate capability
  subscribeToPlutusApp :: PlutusAppId -> m Unit
  subscribeToWallet :: WalletId -> m Unit
  unsubscribeFromPlutusApp :: PlutusAppId -> m Unit
  unsubscribeFromWallet :: WalletId -> m Unit

instance manageMarloweAppM :: ManageMarlowe AppM where
  -- create a MarloweFollower app, call its "follow" endpoint with the given MarloweParams, and then
  -- return its PlutusAppId and observable state
  followContract wallet marloweParams =
    runExceptT do
      let walletId = view _walletId wallet
      contracts <- _.contracts <$> lift getStore

      let
        activateNewFollower = do
          followAppId <- withExceptT followContractError.jsonAjaxError $ ExceptT
            $ PAB.activateContract
                MarloweFollower
                walletId
          void $ withExceptT followContractError.jsonAjaxError $ ExceptT $
            PAB.invokeEndpoint followAppId
              "follow"
              marloweParams
          pure followAppId
      -- If we already have a Follower contract use it, if we don't, activate a new one
      followAppId <- maybe'
        (\_ -> activateNewFollower)
        pure
        (getFollowerContract marloweParams contracts)

      observableStateJson <- withExceptT followContractError.jsonAjaxError
        $ ExceptT
        $ PAB.getContractInstanceObservableState followAppId
      contractHistory <-
        except
          $ lmap followContractError.jsonDecodeError
          $ decodeJson
          $ observableStateJson

      metadata <- ExceptT $ pure
        $ note (followContractError.metadataNotFoundError MetadataNotFoundError)
        $ _.metaData <$> (findTemplate $ getContract contractHistory)

      lift
        $ updateStore
        $ Store.AddFollowerContract followAppId metadata contractHistory
      pure $ followAppId /\ contractHistory

  initializeContract currentInstant template params wallet =
    runExceptT do
      let
        { instantiateContractError, jsonAjaxError } = initializeContractError
        { nickname, roles } = params
        marloweAppId = view _marloweAppId wallet
      -- To initialize a Marlowe Contract we first need to make an instance
      -- of a Core.Marlowe contract. We do this by replazing template parameters
      -- from the Extended.Marlowe template and then calling toCore. This can
      -- fail with `instantiateContractError` if not all params were provided.
      contract <-
        withExceptT instantiateContractError
          $ ExceptT
          $ pure
          $ instantiateExtendedContract
              currentInstant
              template
              params
      -- Call the PAB to create the new contract. It returns a request id and a function
      -- that we can use to block and wait for the response
      reqId /\ awaitContractCreation <-
        withExceptT jsonAjaxError $ ExceptT $
          MarloweApp.createContract marloweAppId roles contract

      -- We save in the store the request of a created contract with
      -- the information relevant to show a placeholder of a starting contract.
      let newContract = NewContract reqId nickname template.metaData
      lift $ updateStore $ Store.AddStartingContract newContract

      pure $ newContract /\ awaitContractCreation

  -- "apply-inputs" to a Marlowe contract on the blockchain
  applyTransactionInput wallet marloweParams transactionInput =
    let
      marloweAppId = view _marloweAppId wallet
    in
      MarloweApp.applyInputs marloweAppId marloweParams transactionInput
  -- "redeem" payments from a Marlowe contract on the blockchain
  redeem wallet marloweParams tokenName =
    let
      marloweAppId = view _marloweAppId wallet

      address = view _address wallet
    in
      MarloweApp.redeem marloweAppId marloweParams tokenName address

  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts wallet =
    runExceptT do
      let
        companionAppId = view _companionAppId wallet
      observableStateJson <- withExceptT Left $ ExceptT $
        PAB.getContractInstanceObservableState companionAppId
      except $ lmap Right $ decodeJson observableStateJson
  subscribeToPlutusApp = Left >>> Subscribe >>> sendWsMessage
  subscribeToWallet =
    sendWsMessage <<< Subscribe <<< Right <<< invalidWalletIdToPubKeyHash
  unsubscribeFromPlutusApp = Left >>> Unsubscribe >>> sendWsMessage
  unsubscribeFromWallet =
    sendWsMessage <<< Unsubscribe <<< Right <<< invalidWalletIdToPubKeyHash

-- | DO NOT USE! This is incorrect. The WS message type _should_ require a
-- | wallet ID, not a pub key hash. This is the cost of using strings for types,
-- | even if wrapped in newtypes!
invalidWalletIdToPubKeyHash :: WalletId -> PubKeyHash
invalidWalletIdToPubKeyHash = PKH.fromString <<< WI.toString

sendWsMessage :: CombinedWSStreamToServer -> AppM Unit
sendWsMessage msg = do
  { pabWebsocket } <- asks $ view _sinks
  liftEffect $ HS.notify pabWebsocket msg

instance ManageMarlowe m => ManageMarlowe (HalogenM state action slots msg m) where
  followContract walletDetails marloweParams = lift $ followContract
    walletDetails
    marloweParams
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName
  getRoleContracts = lift <<< getRoleContracts
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet

instance ManageMarlowe m => ManageMarlowe (MaybeT m) where
  followContract walletDetails marloweParams = lift $ followContract
    walletDetails
    marloweParams
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName
  getRoleContracts = lift <<< getRoleContracts
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet
