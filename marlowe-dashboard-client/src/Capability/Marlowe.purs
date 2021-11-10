module Capability.Marlowe
  ( class ManageMarlowe
  , createWallet
  , followContract
  , createPendingFollowerApp
  , followContractWithPendingFollowerApp
  , createContract
  , applyTransactionInput
  , redeem
  , lookupWalletInfo
  , lookupWalletDetails
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
  , _cicWallet
  , _observableState
  )
import AppM (AppM)
import Bridge (toBack, toFront)
import Capability.Contract
  ( activateContract
  , getContractInstanceClientState
  , getContractInstanceObservableState
  , getWalletContractInstances
  , invokeEndpoint
  ) as Contract
import Capability.Contract (class ManageContract)
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Wallet (class ManageWallet)
import Capability.Wallet (createWallet, getWalletInfo, getWalletTotalFunds) as Wallet
import Component.Contacts.Lenses
  ( _companionAppId
  , _marloweAppId
  , _pubKeyHash
  , _wallet
  , _walletInfo
  )
import Component.Contacts.Types (Wallet, WalletDetails, WalletInfo)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Reader (asks)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Array (filter) as Array
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Lens (view)
import Data.Map (Map, fromFoldable)
import Data.Newtype (unwrap, un)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Halogen (HalogenM, liftAff)
import Marlowe.Client (ContractHistory)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( Contract
  , MarloweData
  , MarloweParams
  , PubKeyHash
  , TokenName
  , TransactionInput
  )
import MarloweContract (MarloweContract(..))
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToServer(..)
  , ContractInstanceClientState
  )
import Plutus.V1.Ledger.Crypto (PubKeyHash(..)) as Back
import Types (AjaxResponse, DecodedAjaxResponse, NotFoundAjaxResponse)
import Wallet.Emulator.Wallet (Wallet(..)) as Back
import WebSocket.Support as WS

-- The `ManageMarlowe` class provides a window on the `ManageContract` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ( ManageContract m
  , ManageMarloweStorage m
  , ManageWallet m
  ) <=
  ManageMarlowe m where
  createWallet :: m (AjaxResponse WalletDetails)
  followContract
    :: WalletDetails
    -> MarloweParams
    -> m (DecodedAjaxResponse (Tuple PlutusAppId ContractHistory))
  createPendingFollowerApp :: WalletDetails -> m (AjaxResponse PlutusAppId)
  followContractWithPendingFollowerApp
    :: WalletDetails
    -> MarloweParams
    -> PlutusAppId
    -> m (DecodedAjaxResponse (Tuple PlutusAppId ContractHistory))
  createContract
    :: WalletDetails
    -> Map TokenName PubKeyHash
    -> Contract
    -> m (AjaxResponse Unit)
  applyTransactionInput
    :: WalletDetails
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse Unit)
  redeem :: WalletDetails -> MarloweParams -> TokenName -> m (AjaxResponse Unit)
  lookupWalletInfo :: PlutusAppId -> m (NotFoundAjaxResponse WalletInfo)
  lookupWalletDetails :: PlutusAppId -> m (NotFoundAjaxResponse WalletDetails)
  getRoleContracts
    :: WalletDetails -> m (DecodedAjaxResponse (Map MarloweParams MarloweData))
  getFollowerApps
    :: WalletDetails
    -> m (DecodedAjaxResponse (Map PlutusAppId ContractHistory))
  subscribeToPlutusApp :: PlutusAppId -> m Unit
  subscribeToWallet :: Wallet -> m Unit
  unsubscribeFromPlutusApp :: PlutusAppId -> m Unit
  unsubscribeFromWallet :: Wallet -> m Unit

instance manageMarloweAppM :: ManageMarlowe AppM where
  createWallet = do
    -- create the wallet itself
    ajaxWalletInfo <- Wallet.createWallet
    case ajaxWalletInfo of
      Left ajaxError -> pure $ Left ajaxError
      Right walletInfo -> do
        let
          wallet = view _wallet walletInfo
        -- create the WalletCompanion and MarloweApp for this wallet
        ajaxCompanionAppId <- Contract.activateContract WalletCompanion wallet
        ajaxMarloweAppId <- Contract.activateContract MarloweApp wallet
        -- get the wallet's current funds
        -- Note that, because it can take a moment for the initial demo funds to be added, at
        -- this point the funds might be zero. It doesn't matter though - if we connect this
        -- wallet, we'll get a WebSocket notification when the funds are added (and if we don't
        -- connect it, we don't need to know what they are.)
        -- TODO(?): Because of that, we could potentially forget about this call and just set
        -- assets to `mempty`.
        ajaxAssets <- Wallet.getWalletTotalFunds wallet
        let
          createWalletDetails companionAppId marloweAppId assets =
            { walletNickname: ""
            , companionAppId
            , marloweAppId
            , walletInfo
            , assets
            , previousCompanionAppState: Nothing
            }
        pure $ createWalletDetails <$> ajaxCompanionAppId <*> ajaxMarloweAppId
          <*> ajaxAssets
  -- create a MarloweFollower app, call its "follow" endpoint with the given MarloweParams, and then
  -- return its PlutusAppId and observable state
  followContract walletDetails marloweParams =
    runExceptT do
      let
        wallet = view (_walletInfo <<< _wallet) walletDetails
      followAppId <- withExceptT Left $ ExceptT $ Contract.activateContract
        MarloweFollower
        wallet
      void $ withExceptT Left $ ExceptT $ Contract.invokeEndpoint followAppId
        "follow"
        marloweParams
      observableStateJson <- withExceptT Left $ ExceptT $
        Contract.getContractInstanceObservableState followAppId
      observableState <-
        except
          $ lmap Right
          $ parseDecodeJson
          $ unwrap observableStateJson
      pure $ followAppId /\ observableState
  -- create a MarloweFollower app and return its PlutusAppId, but don't call its "follow" endpoint
  -- (this function is used for creating "placeholder" contracts before we know the MarloweParams)
  createPendingFollowerApp walletDetails =
    let
      wallet = view (_walletInfo <<< _wallet) walletDetails
    in
      Contract.activateContract MarloweFollower wallet
  -- call the "follow" endpoint of a pending MarloweFollower app, and return its PlutusAppId and
  -- observable state (to call this function, we must already know its PlutusAppId, but we return
  -- it anyway because it is convenient to have this function return the same type as
  -- `followContract`)
  followContractWithPendingFollowerApp _ marloweParams followerAppId =
    runExceptT do
      void $ withExceptT Left $ ExceptT $ Contract.invokeEndpoint followerAppId
        "follow"
        marloweParams
      observableStateJson <-
        withExceptT Left $ ExceptT $ Contract.getContractInstanceObservableState
          followerAppId
      observableState <-
        except
          $ lmap Right
          $ parseDecodeJson
          $ unwrap observableStateJson
      pure $ followerAppId /\ observableState
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
  applyTransactionInput walletDetails marloweParams transactionInput =
    let
      marloweAppId = view _marloweAppId walletDetails
    in
      MarloweApp.applyInputs marloweAppId marloweParams transactionInput
  -- "redeem" payments from a Marlowe contract on the blockchain
  redeem walletDetails marloweParams tokenName =
    let
      marloweAppId = view _marloweAppId walletDetails

      pubKeyHash = view (_walletInfo <<< _pubKeyHash) walletDetails
    in
      MarloweApp.redeem marloweAppId marloweParams tokenName pubKeyHash
  -- get the WalletInfo of a wallet given the PlutusAppId of its WalletCompanion
  lookupWalletInfo companionAppId =
    runExceptT do
      clientState <- withExceptT Just $ ExceptT $
        Contract.getContractInstanceClientState companionAppId
      case view _cicDefinition clientState of
        WalletCompanion -> withExceptT Just $ ExceptT $ Wallet.getWalletInfo
          $ toFront
          $ view _cicWallet clientState
        _ -> except $ Left $ Nothing
  -- get the WalletDetails of a wallet given the PlutusAppId of its WalletCompanion
  -- note: this returns an empty walletNickname (because these are only saved locally)
  lookupWalletDetails companionAppId =
    runExceptT do
      clientState <- withExceptT Just $ ExceptT $
        Contract.getContractInstanceClientState companionAppId
      case view _cicDefinition clientState of
        WalletCompanion -> do
          let
            wallet = toFront $ view _cicWallet clientState
          walletContracts <- withExceptT Just $ ExceptT $
            Contract.getWalletContractInstances wallet
          walletInfo <- withExceptT Just $ ExceptT $ Wallet.getWalletInfo wallet
          assets <- withExceptT Just $ ExceptT $ Wallet.getWalletTotalFunds
            wallet
          case
            find (\state -> view _cicDefinition state == MarloweApp)
              walletContracts
            of
            Just marloweApp ->
              pure
                { walletNickname: mempty
                , companionAppId
                , marloweAppId: toFront $ view _cicContract marloweApp
                , walletInfo
                , assets
                , previousCompanionAppState: Nothing
                }
            Nothing -> except $ Left Nothing
        _ -> except $ Left Nothing
  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts walletDetails =
    runExceptT do
      let
        companionAppId = view _companionAppId walletDetails
      observableStateJson <- withExceptT Left $ ExceptT $
        Contract.getContractInstanceObservableState companionAppId
      except $ lmap Right $ parseDecodeJson $ unwrap observableStateJson
  -- get all MarloweFollower apps for a given wallet
  getFollowerApps walletDetails =
    runExceptT do
      let
        wallet = view (_walletInfo <<< _wallet) walletDetails
      runningApps <- withExceptT Left $ ExceptT $
        Contract.getWalletContractInstances wallet
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
        plutusAppId = toFront $ view _cicContract contractInstanceClientState

        rawJson = view (_cicCurrentState <<< _observableState)
          contractInstanceClientState
      in
        case parseDecodeJson $ unwrap rawJson of
          Left decodingErrors -> Left decodingErrors
          Right observableState -> Right (plutusAppId /\ observableState)
  subscribeToPlutusApp = toBack >>> Left >>> Subscribe >>> sendWsMessage
  subscribeToWallet =
    toBack
      >>> un Back.Wallet
      >>> _.getWalletId
      >>> (\x -> Back.PubKeyHash { getPubKeyHash: x })
      >>> Right
      >>> Subscribe
      >>> sendWsMessage
  unsubscribeFromPlutusApp = toBack >>> Left >>> Unsubscribe >>> sendWsMessage
  unsubscribeFromWallet =
    toBack
      >>> un Back.Wallet
      >>> _.getWalletId
      >>> (\x -> Back.PubKeyHash { getPubKeyHash: x })
      >>> Right
      >>> Unsubscribe
      >>> sendWsMessage

sendWsMessage :: CombinedWSStreamToServer -> AppM Unit
sendWsMessage msg = do
  wsManager <- asks _.wsManager
  liftAff
    $ WS.managerWriteOutbound wsManager
    $ WS.SendMessage msg

instance monadMarloweHalogenM ::
  ( ManageMarlowe m
  ) =>
  ManageMarlowe (HalogenM state action slots msg m) where
  createWallet = lift createWallet
  followContract walletDetails marloweParams = lift $ followContract
    walletDetails
    marloweParams
  createPendingFollowerApp = lift <<< createPendingFollowerApp
  followContractWithPendingFollowerApp walletDetails marloweParams followAppId =
    lift $ followContractWithPendingFollowerApp walletDetails marloweParams
      followAppId
  createContract walletDetails roles contract = lift $ createContract
    walletDetails
    roles
    contract
  applyTransactionInput walletDetails marloweParams transactionInput = lift $
    applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName = lift $ redeem walletDetails
    marloweParams
    tokenName
  lookupWalletInfo = lift <<< lookupWalletInfo
  lookupWalletDetails = lift <<< lookupWalletDetails
  getRoleContracts = lift <<< getRoleContracts
  getFollowerApps = lift <<< getFollowerApps
  subscribeToPlutusApp = lift <<< subscribeToPlutusApp
  subscribeToWallet = lift <<< subscribeToWallet
  unsubscribeFromPlutusApp = lift <<< unsubscribeFromPlutusApp
  unsubscribeFromWallet = lift <<< unsubscribeFromWallet
