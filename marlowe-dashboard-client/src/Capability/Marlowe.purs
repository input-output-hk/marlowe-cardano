module Capability.Marlowe
  ( class ManageMarlowe
  , followContract
  , createContract
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
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (asks)
import Data.Address (Address)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.Either (note)
import Data.Lens (view)
import Data.Map (Map)
import Data.Maybe (maybe')
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
import Data.UUID.Argonaut (UUID)
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
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( Contract
  , MarloweData
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
  createContract
    :: PABConnectedWallet
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse (UUID /\ Aff MarloweParams))
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
  createContract walletDetails roles contract =
    lift $ createContract walletDetails roles contract
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
  createContract walletDetails roles contract =
    lift $ createContract walletDetails roles contract
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName
  getRoleContracts = lift <<< getRoleContracts
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet
