module Capability.Marlowe
  ( class ManageMarlowe
  , createWallet
  , restoreWallet
  , followContract
  , createPendingFollowerApp
  , followContractWithPendingFollowerApp
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
  , _cicWallet
  , _observableState
  )
import API.Marlowe.Run.Wallet.CentralizedTestnet (RestoreError(..))
import AppM (AppM)
import Bridge (toBack, toFront)
import Capability.PAB (class ManagePAB)
import Capability.PAB
  ( activateContract
  , getContractInstanceClientState
  , getContractInstanceObservableState
  , getWalletContractInstances
  , invokeEndpoint
  ) as PAB
import Capability.MarloweStorage (class ManageMarloweStorage)
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Wallet (class ManageWallet)
import Capability.Wallet as Wallet
import Component.Contacts.Lenses
  ( _companionAppId
  , _marloweAppId
  , _pubKeyHash
  , _walletId
  , _walletInfo
  )
import Component.Contacts.Types (WalletDetails, WalletId)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Reader (asks)
import Data.Address (Address)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Array (filter) as Array
import Data.Array (find)
import Data.Bifunctor (lmap)
import Data.Lens (view)
import Data.Map (Map, fromFoldable)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.Newtype (un, unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Env (Env(..))
import Halogen (HalogenM, liftAff)
import Marlowe.Client (ContractHistory)
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
import Plutus.V1.Ledger.Crypto (PubKeyHash(..)) as Back
import Types (AjaxResponse, DecodedAjaxResponse, NotFoundAjaxResponse)
import Wallet.Emulator.Wallet (Wallet(..)) as Back
import WebSocket.Support as WS

-- The `ManageMarlowe` class provides a window on the `ManagePAB` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ( ManagePAB m
  , ManageMarloweStorage m
  , ManageWallet m
  ) <=
  ManageMarlowe m where
  createWallet :: m (AjaxResponse WalletDetails)
  restoreWallet
    :: WalletNickname
    -> MnemonicPhrase
    -> String
    -> m (Either RestoreError WalletDetails)
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
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse Unit)
  applyTransactionInput
    :: WalletDetails
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse Unit)
  redeem :: WalletDetails -> MarloweParams -> TokenName -> m (AjaxResponse Unit)
  getRoleContracts
    :: WalletDetails -> m (DecodedAjaxResponse (Map MarloweParams MarloweData))
  getFollowerApps
    :: WalletDetails
    -> m (DecodedAjaxResponse (Map PlutusAppId ContractHistory))
  subscribeToPlutusApp :: PlutusAppId -> m Unit
  subscribeToWallet :: WalletId -> m Unit
  unsubscribeFromPlutusApp :: PlutusAppId -> m Unit
  unsubscribeFromWallet :: WalletId -> m Unit

instance manageMarloweAppM :: ManageMarlowe AppM where
  -- TODO: This code was meant for mock wallet, as part of SCP-3170 we should re-implement this
  --       using the WBE.
  createWallet = do
    -- create the wallet itself
    ajaxWalletInfo <- Wallet.createWallet
    case ajaxWalletInfo of
      Left ajaxError -> pure $ Left ajaxError
      Right walletInfo -> do
        let
          walletId = view _walletId walletInfo
        -- create the WalletCompanion and MarloweApp for this wallet
        ajaxCompanionAppId <- PAB.activateContract WalletCompanion walletId
        ajaxMarloweAppId <- PAB.activateContract MarloweApp walletId
        let
          createWalletDetails companionAppId marloweAppId =
            { walletNickname: WN.new
            , companionAppId
            , marloweAppId
            , walletInfo
            , assets: mempty
            }
        pure $ createWalletDetails <$> ajaxCompanionAppId <*> ajaxMarloweAppId
  restoreWallet walletName mnemonicPhrase passphrase = do
    mWalletInfo <- Wallet.restoreWallet
      { walletName: WN.toString walletName
      , mnemonicPhrase: map MP.wordToString $ MP.toWords mnemonicPhrase
      , passphrase
      }
    case mWalletInfo of
      Left err -> pure $ Left err
      Right walletInfo -> do
        let
          walletId = view _walletId walletInfo
        -- create the WalletCompanion and MarloweApp for this wallet
        -- TODO: We had initial discussions for adding Variants for errors probably using Checked Exceptions.
        -- Instead of the general ClientServerError we could encapsulate the type of error inside a context
        -- (restoring the wallet, activating a wallet companion or marlowe app).
        -- For the end users this wouldn't change too much their UX, but for production code it can be useful
        -- to diagnose problems using a tool like sentry SCP-3171.
        -- https://github.com/natefaubion/purescript-checked-exceptions
        ajaxCompanionAppId <- lmap ClientServerError <$>
          PAB.activateContract WalletCompanion walletId
        ajaxMarloweAppId <- lmap ClientServerError <$> PAB.activateContract
          MarloweApp
          walletId
        let
          createWalletDetails companionAppId marloweAppId =
            { walletNickname: walletName
            , companionAppId
            , marloweAppId
            , walletInfo
            , assets: mempty
            }
        pure $ createWalletDetails <$> ajaxCompanionAppId <*> ajaxMarloweAppId
  -- create a MarloweFollower app, call its "follow" endpoint with the given MarloweParams, and then
  -- return its PlutusAppId and observable state
  followContract walletDetails marloweParams =
    runExceptT do
      let
        walletId = view (_walletInfo <<< _walletId) walletDetails
      followAppId <- withExceptT Left $ ExceptT $ PAB.activateContract
        MarloweFollower
        walletId
      void $ withExceptT Left $ ExceptT $ PAB.invokeEndpoint followAppId
        "follow"
        marloweParams
      observableStateJson <- withExceptT Left $ ExceptT $
        PAB.getContractInstanceObservableState followAppId
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
      walletId = view (_walletInfo <<< _walletId) walletDetails
    in
      PAB.activateContract MarloweFollower walletId
  -- call the "follow" endpoint of a pending MarloweFollower app, and return its PlutusAppId and
  -- observable state (to call this function, we must already know its PlutusAppId, but we return
  -- it anyway because it is convenient to have this function return the same type as
  -- `followContract`)
  followContractWithPendingFollowerApp _ marloweParams followerAppId =
    runExceptT do
      void $ withExceptT Left $ ExceptT $ PAB.invokeEndpoint followerAppId
        "follow"
        marloweParams
      observableStateJson <-
        withExceptT Left $ ExceptT $ PAB.getContractInstanceObservableState
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

      address = view (_walletInfo <<< _pubKeyHash) walletDetails
    in
      MarloweApp.redeem marloweAppId marloweParams tokenName address

  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts walletDetails =
    runExceptT do
      let
        companionAppId = view _companionAppId walletDetails
      observableStateJson <- withExceptT Left $ ExceptT $
        PAB.getContractInstanceObservableState companionAppId
      except $ lmap Right $ parseDecodeJson $ unwrap observableStateJson
  -- get all MarloweFollower apps for a given wallet
  getFollowerApps walletDetails =
    runExceptT do
      let
        walletId = view (_walletInfo <<< _walletId) walletDetails
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
  wsManager <- asks \(Env e) -> e.wsManager
  liftAff
    $ WS.managerWriteOutbound wsManager
    $ WS.SendMessage msg

instance monadMarloweHalogenM ::
  ( ManageMarlowe m
  ) =>
  ManageMarlowe (HalogenM state action slots msg m) where
  createWallet = lift createWallet
  restoreWallet = map (map (map lift)) restoreWallet
  followContract walletDetails marloweParams = lift $ followContract
    walletDetails
    marloweParams
  createPendingFollowerApp = lift <<< createPendingFollowerApp
  followContractWithPendingFollowerApp walletDetails marloweParams followAppId =
    lift $ followContractWithPendingFollowerApp
      walletDetails
      marloweParams
      followAppId
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
