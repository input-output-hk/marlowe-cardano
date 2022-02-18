module Capability.Marlowe
  ( class ManageMarlowe
  , followContract
  , createContract
  , applyTransactionInput
  , redeem
  , getRoleContracts
  , getFollowerApps
  , subscribeToWallet
  , unsubscribeFromWallet
  , subscribeToPlutusApp
  , unsubscribeFromPlutusApp
  ) where

import Prologue

import API.Lenses
  ( _cicContract
  , _cicCurrentState
  , _cicDefinition
  , _observableState
  )
import AppM (AppM)
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.PAB (class ManagePAB)
import Capability.PAB
  ( activateContract
  , getContractInstanceObservableState
  , getWalletContractInstances
  , invokeEndpoint
  ) as PAB
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Wallet (class ManageWallet)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Reader (asks)
import Data.Address (Address)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Array (filter) as Array
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Lens (view)
import Data.Map (Map, fromFoldable)
import Data.Maybe (maybe')
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _companionAppId
  , _marloweAppId
  , _pubKeyHash
  , _walletId
  )
import Data.PaymentPubKeyHash (_PaymentPubKeyHash)
import Data.PubKeyHash (PubKeyHash)
import Data.PubKeyHash as PKH
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Data.Variant (Variant)
import Data.Variant.Generic (class Constructors, mkConstructors')
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Env (Env(..))
import Halogen (HalogenM, liftAff)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Marlowe.Client (ContractHistory, getContract)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( Contract
  , MarloweData
  , MarloweParams
  , TokenName
  , TransactionInput
  )
import MarloweContract (MarloweContract(..))
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToServer(..)
  , ContractInstanceClientState
  )
import Store as Store
import Store.Contracts (getFollowerContract)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Types
  ( AjaxResponse
  , DecodedAjaxResponse
  , JsonAjaxErrorRow
  , JsonDecodeErrorRow
  , MetadataNotFoundErrorRow
  )
import WebSocket.Support as WS

type FollowContractError = Variant
  (JsonAjaxErrorRow + MetadataNotFoundErrorRow + JsonDecodeErrorRow + ())

followContractError :: forall c. Constructors FollowContractError c => c
followContractError = mkConstructors' (Proxy :: Proxy FollowContractError)

-- The `ManageMarlowe` class provides a window on the `ManagePAB` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ( ManagePAB m
  , ManageMarloweStorage m
  , ManageWallet m
  , MonadStore Store.Action Store.Store m
  ) <=
  ManageMarlowe m where
  followContract
    :: PABConnectedWallet
    -> MarloweParams
    -> m (Either FollowContractError (PlutusAppId /\ ContractHistory))
  createContract
    :: PABConnectedWallet
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse UUID)
  applyTransactionInput
    :: PABConnectedWallet
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse Unit)
  redeem
    :: PABConnectedWallet -> MarloweParams -> TokenName -> m (AjaxResponse Unit)
  getRoleContracts
    :: PABConnectedWallet
    -> m (DecodedAjaxResponse (Map MarloweParams MarloweData))
  getFollowerApps
    :: WalletId
    -> m (DecodedAjaxResponse (Map PlutusAppId ContractHistory))

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
      let
        walletId = view _walletId wallet
      contracts /\ currentSlot <-
        (\store -> store.contracts /\ store.currentSlot) <$> lift getStore

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

      let
        contract = getContract contractHistory

      metadata <- ExceptT $ pure
        $ note followContractError.metadataNotFoundError
        $ _.metaData <$> findTemplate contract

      lift $ updateStore $ Store.AddFollowerContract currentSlot followAppId
        metadata
        contractHistory
      pure $ followAppId /\ contractHistory
  -- "create" a Marlowe contract on the blockchain
  -- FIXME: if we want users to be able to follow contracts that they don't have roles in, we need this function
  -- to return the MarloweParams of the created contract - but this isn't currently possible in the PAB
  -- UPDATE to this FIXME: it is possible this won't be a problem, as it seems role tokens are first paid into
  -- the wallet that created the contract, and distributed to other wallets from there - but this remains to be
  -- seen when all the parts are working together as they should be...
  createContract walletDetails roles contract =
    let
      marloweAppId = view _marloweAppId walletDetails
    in
      MarloweApp.createContract marloweAppId roles contract
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

      pubKeyHash = view
        (_pubKeyHash <<< _PaymentPubKeyHash)
        wallet
    in
      MarloweApp.redeem marloweAppId marloweParams tokenName pubKeyHash

  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts wallet =
    runExceptT do
      let
        companionAppId = view _companionAppId wallet
      observableStateJson <- withExceptT Left $ ExceptT $
        PAB.getContractInstanceObservableState companionAppId
      except $ lmap Right $ decodeJson observableStateJson
  -- get all MarloweFollower apps for a given wallet
  getFollowerApps walletId =
    runExceptT do
      runningApps <- withExceptT Left $ ExceptT $
        PAB.getWalletContractInstances walletId
      let
        followerApps = Array.filter
          (\cic -> view _cicDefinition cic == MarloweFollower)
          runningApps
      case traverse decodeFollowerAppState followerApps of
        Left decodingError -> except $ Left $ Right decodingError
        Right decodedFollowerApps -> ExceptT $ pure $ Right $ fromFoldable
          decodedFollowerApps
    where
    decodeFollowerAppState
      :: ContractInstanceClientState MarloweContract
      -> Either JsonDecodeError (Tuple PlutusAppId ContractHistory)
    decodeFollowerAppState contractInstanceClientState =
      let
        plutusAppId = view _cicContract contractInstanceClientState

        rawJson = view (_cicCurrentState <<< _observableState)
          contractInstanceClientState
      in
        case decodeJson rawJson of
          Left decodingErrors -> Left decodingErrors
          Right observableState -> Right (plutusAppId /\ observableState)
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
  wsManager <- asks \(Env e) -> e.wsManager
  liftAff
    $ WS.managerWriteOutbound wsManager
    $ WS.SendMessage msg

instance monadMarloweHalogenM ::
  ( ManageMarlowe m
  ) =>
  ManageMarlowe (HalogenM state action slots msg m) where
  followContract walletDetails marloweParams = lift $ followContract
    walletDetails
    marloweParams
  createContract walletDetails roles contract =
    lift $ createContract walletDetails roles contract
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName
  getRoleContracts = lift <<< getRoleContracts
  getFollowerApps = lift <<< getFollowerApps
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet
