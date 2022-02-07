module Capability.Marlowe
  ( class ManageMarlowe
  , createWallet
  , restoreWallet
  , followContract
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
  , _cicStatus
  , _observableState
  )
import API.Marlowe.Run.Wallet.CentralizedTestnet
  ( ClientServerErrorRow
  , CreateWalletError
  , RestoreWalletError
  , clientServerError
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
import Capability.Wallet as Wallet
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Reader (asks)
import Data.Argonaut.Decode (JsonDecodeError, decodeJson)
import Data.Array (filter, find) as Array
import Data.Bifunctor (lmap)
import Data.Lens (view)
import Data.Map (Map, fromFoldable)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.MnemonicPhrase.Word (toString) as Word
import Data.Passpharse (Passphrase)
import Data.PaymentPubKeyHash (_PaymentPubKeyHash)
import Data.PubKeyHash (PubKeyHash)
import Data.PubKeyHash as PKH
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut (UUID)
import Data.Variant (Variant)
import Data.Wallet
  ( WalletDetails
  , _companionAppId
  , _marloweAppId
  , _pubKeyHash
  , _walletId
  , mkWalletDetails
  )
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Data.WalletNickname (WalletNickname)
import Env (Env(..))
import Halogen (HalogenM, liftAff)
import Marlowe.Client (ContractHistory)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Marlowe.Semantics
  ( Contract
  , MarloweData
  , MarloweParams
  , TokenName
  , TransactionInput
  )
import MarloweContract (MarloweContract(..))
import Page.Welcome.Types (NewWalletDetails)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToServer(..)
  , ContractInstanceClientState
  )
import Types (AjaxResponse, DecodedAjaxResponse)
import Wallet.Types (ContractActivityStatus(..))
import WebSocket.Support as WS

-- The `ManageMarlowe` class provides a window on the `ManagePAB` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ( ManagePAB m
  , ManageMarloweStorage m
  , ManageWallet m
  ) <=
  ManageMarlowe m where
  createWallet
    :: WalletNickname
    -> Passphrase
    -> m (Either CreateWalletError NewWalletDetails)
  restoreWallet
    :: WalletNickname
    -> MnemonicPhrase
    -> Passphrase
    -> m (Either RestoreWalletError WalletDetails)
  followContract
    :: WalletDetails
    -> MarloweParams
    -> m (DecodedAjaxResponse (Tuple PlutusAppId ContractHistory))
  followContractWithPendingFollowerApp
    :: WalletDetails
    -> MarloweParams
    -> PlutusAppId
    -> m (DecodedAjaxResponse (Tuple PlutusAppId ContractHistory))
  createContract
    :: WalletDetails
    -> Map TokenName PubKeyHash
    -> Contract
    -> m (AjaxResponse UUID)
  applyTransactionInput
    :: WalletDetails
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse Unit)
  redeem :: WalletDetails -> MarloweParams -> TokenName -> m (AjaxResponse Unit)
  getRoleContracts
    :: WalletDetails -> m (DecodedAjaxResponse (Map MarloweParams MarloweData))
  getFollowerApps
    :: WalletId
    -> m (DecodedAjaxResponse (Map PlutusAppId ContractHistory))
  subscribeToPlutusApp :: PlutusAppId -> m Unit
  subscribeToWallet :: WalletId -> m Unit
  unsubscribeFromPlutusApp :: PlutusAppId -> m Unit
  unsubscribeFromWallet :: WalletId -> m Unit

{- [UC-WALLET-TESTNET-2][3] Restore a testnet wallet
  After receiving the WalletId from the backend, we need to fetch the plutus
  contracts this wallet might have, and see if there is already an instance of
  the WalletCompanion and MarloweApp plutus scripts.
  TODO: there are two possible flows to restore a wallet, (search 4a and 4b), in
        step 5 we are subscribing again to the plutus scripts. While I think it doesn't
        cause a bug, it is a little bit weird. We should modify WalletDetails to have the
        concept of "connected/disconnected", and connect in step 5 rather than here.
-}
fetchWalletDetails
  :: forall m r
   . ManageMarlowe m
  => { newWallet :: Boolean
     , walletNickname :: WalletNickname
     , walletInfo :: WalletInfo
     }
  -> ExceptT (Variant (ClientServerErrorRow r)) m WalletDetails
fetchWalletDetails { newWallet, walletNickname, walletInfo } = withExceptT
  clientServerError
  do
    let
      WalletInfo { walletId } = walletInfo

    contracts <-
      if newWallet then pure []
      else
        ExceptT $
          PAB.getWalletContractInstances walletId

    -- If we already have the plutus contract for the wallet companion and marlowe app
    -- let's use those, if note activate new instances of them
    { companionAppId, marloweAppId } <- ExceptT $
      activateOrRestorePlutusCompanionContracts walletId contracts
    -- TODO (as part of SCP-3360):
    --   create a list of "loading contracts" with the plutusContracts of type MarloweFollower
    pure $ mkWalletDetails walletNickname companionAppId marloweAppId walletInfo

instance manageMarloweAppM :: ManageMarlowe AppM where
  createWallet walletNickname passphrase = runExceptT do
    -- create the wallet itself
    { mnemonic, walletInfo } <- ExceptT $ Wallet.createWallet walletNickname
      passphrase
    walletDetails <- fetchWalletDetails
      { newWallet: true, walletNickname, walletInfo }
    pure { mnemonic, walletDetails }
  restoreWallet walletNickname mnemonicPhrase passphrase = runExceptT do
    walletInfo <- ExceptT $ Wallet.restoreWallet
      { walletName: walletNickname
      , mnemonicPhrase: map Word.toString $ MP.toWords mnemonicPhrase
      , passphrase
      }
    -- Get all plutus contracts associated with the restored wallet.
    fetchWalletDetails { walletNickname, walletInfo, newWallet: false }

  -- create a MarloweFollower app, call its "follow" endpoint with the given MarloweParams, and then
  -- return its PlutusAppId and observable state
  followContract walletDetails marloweParams =
    runExceptT do
      let
        walletId = view _walletId walletDetails
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
          $ decodeJson
          $ observableStateJson
      pure $ followAppId /\ observableState
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
          $ decodeJson
          $ observableStateJson
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

      pubKeyHash = view
        (_pubKeyHash <<< _PaymentPubKeyHash)
        walletDetails
    in
      MarloweApp.redeem marloweAppId marloweParams tokenName pubKeyHash

  -- get the observable state of a wallet's WalletCompanion
  getRoleContracts walletDetails =
    runExceptT do
      let
        companionAppId = view _companionAppId walletDetails
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

-- Helper function to the restoreWallet so that we can reutilize the wallet companion or marlowe app if available
activateOrRestorePlutusCompanionContracts
  :: forall m
   . ManagePAB m
  => WalletId
  -> Array (ContractInstanceClientState MarloweContract)
  -> m
       ( AjaxResponse
           { companionAppId :: PlutusAppId, marloweAppId :: PlutusAppId }
       )
activateOrRestorePlutusCompanionContracts walletId plutusContracts = runExceptT
  do
    let
      isActiveContract contractType cic =
        let
          definition = view _cicDefinition cic
          status = view _cicStatus cic
        in
          definition == contractType && status == Active

      findOrActivateContract contractType =
        -- Try to find the contract by its type
        Array.find (isActiveContract contractType) plutusContracts # case _ of
          Nothing ->
            -- If we cannot find it, activate a new one
            ExceptT $ PAB.activateContract contractType walletId
          Just contract ->
            -- If we find it, return the id
            pure $ view _cicContract contract
    { companionAppId: _, marloweAppId: _ }
      <$> findOrActivateContract WalletCompanion
      <*> findOrActivateContract MarloweApp

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
  createWallet wn p = lift $ createWallet wn p
  restoreWallet = map (map (map lift)) restoreWallet
  followContract walletDetails marloweParams = lift $ followContract
    walletDetails
    marloweParams
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
