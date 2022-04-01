module Capability.Marlowe
  ( ApplyInputError
  , CreateError(..)
  , applyTransactionInput
  , class ManageMarlowe
  , initializeContract
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Capability.PlutusApps.MarloweApp (applyInputs, createContract, redeem) as MarloweApp
import Capability.Toast (addToast)
import Capability.Wallet (class ManageWallet)
import Component.ContractSetup.Types (ContractParams)
import Component.Template.State
  ( InstantiateContractErrorRow
  , instantiateExtendedContract
  )
import Control.Logger.Structured (info, info')
import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.UUID (class MonadUUID)
import Data.Argonaut (encodeJson)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant)
import Data.Lens (view)
import Data.NewContract (NewContract(..))
import Data.PABConnectedWallet (PABConnectedWallet, _address, _marloweAppId)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Generic (class Constructors, mkConstructors')
import Effect.Aff (Aff, Error, error, forkAff, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Unlift (class MonadUnliftAff, askUnliftAff, unliftAff)
import Errors.Debuggable (class Debuggable)
import Errors.Explain (class Explain)
import Halogen (HalogenM)
import Halogen.Store.Monad (updateStore)
import Language.Marlowe.Client (MarloweError)
import Language.Marlowe.Client.Error (showContractError)
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.Run.Server (Api) as MarloweApp
import Marlowe.Semantics (MarloweParams, TokenName, TransactionInput)
import Plutus.PAB.Webserver (Api) as PAB
import Servant.PureScript (class MonadAjax)
import Store as Store
import Text.Pretty (text)
import Toast.Types (errorToast)
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Types (AjaxResponse, JsonAjaxErrorRow)

type InitializeContractError = Variant
  (JsonAjaxErrorRow + InstantiateContractErrorRow + ())

initializeContractError
  :: forall c. Constructors InitializeContractError c => c
initializeContractError = mkConstructors'
  (Proxy :: Proxy InitializeContractError)

-- The `ManageMarlowe` class provides a window on the `ManagePAB` and `ManageWallet`
-- capabilities with functions specific to Marlowe.
class
  ManageWallet m <=
  ManageMarlowe m where
  initializeContract
    :: Instant
    -> ContractTemplate
    -> ContractParams
    -> PABConnectedWallet
    -> m
         ( Either
             InitializeContractError
             (NewContract /\ Aff (Either CreateError MarloweParams))
         )
  applyTransactionInput
    :: PABConnectedWallet
    -> MarloweParams
    -> TransactionInput
    -> m (AjaxResponse (Aff (Either ApplyInputError Unit)))
  redeem
    :: PABConnectedWallet
    -> MarloweParams
    -> TokenName
    -> m (AjaxResponse (Aff Unit))

instance
  ( MonadUnliftAff m
  , MonadError Error m
  , MonadRec m
  , MonadAjax PAB.Api m
  , MonadAjax MarloweApp.Api m
  , MonadUUID m
  ) =>
  ManageMarlowe (AppM m) where

  initializeContract currentInstant template params wallet = do
    info' "Initializing contract"
    u <- askUnliftAff
    runExceptT do
      let
        { instantiateContractError, jsonAjaxError } = initializeContractError
        { nickname, roles } = params
        marloweAppId = view _marloweAppId wallet
      -- To initialize a Marlowe Contract we first need to make an instance
      -- of a Core.Marlowe contract. We do this by replazing template parameters
      -- from the Extended.Marlowe template and then calling toCore. This can
      -- fail with `instantiateContractError` if not all params were provided.
      contract <- except
        $ lmap instantiateContractError
        $ instantiateExtendedContract currentInstant template params
      -- Call the PAB to create the new contract. It returns a request id and a function
      -- that we can use to block and wait for the response
      reqId /\ awaitContractCreation <-
        withExceptT jsonAjaxError $ ExceptT $
          MarloweApp.createContract marloweAppId roles contract

      -- We save in the store the request of a created contract with
      -- the information relevant to show a placeholder of a starting contract.
      let newContract = NewContract reqId nickname template.metaData Nothing
      lift $ updateStore $ Store.ContractCreated newContract

      -- Already fork and await the pending result here, so we don't have to
      -- wait for the caller to run it to update the store.
      resultFiber <- liftAff $ forkAff do
        mParams <- awaitContractCreation
        -- Update the contract's representation in the store to use its
        -- MarloweParams if successful, or show an error otherwise.
        unliftAff u case mParams of
          Left contractError -> do
            updateStore $ Store.ContractStartFailed newContract contractError
            pure $ Left $ CreateError contractError
          Right marloweParams -> do
            updateStore $ Store.ContractStarted newContract marloweParams
            pure $ Right marloweParams

      pure $ newContract /\ joinFiber resultFiber

  -- "apply-inputs" to a Marlowe contract on the blockchain
  applyTransactionInput wallet marloweParams transactionInput = do
    info "Applying input " $ encodeJson { marloweParams, transactionInput }
    let
      marloweAppId = view _marloweAppId wallet

      wrapError
        :: AjaxResponse (Aff (Either MarloweError Unit))
        -> AjaxResponse (Aff (Either ApplyInputError Unit))
      wrapError = map $ map $ lmap ApplyInputError
    MarloweApp.applyInputs marloweAppId marloweParams transactionInput
      -- We wrap the MarloweError in an Explainable and Debuggable context
      <#> wrapError

  -- "redeem" payments from a Marlowe contract on the blockchain
  redeem wallet marloweParams tokenName = do
    info "Redeeming payments" $ encodeJson { marloweParams, tokenName }
    u <- askUnliftAff
    runExceptT do
      let marloweAppId = view _marloweAppId wallet
      let address = view _address wallet
      awaitResult <- ExceptT
        $ MarloweApp.redeem marloweAppId marloweParams tokenName address
      pure do
        mUnit <- awaitResult
        case mUnit of
          Left contractError -> do
            unliftAff u
              $ addToast
              $ errorToast "Failed to redeem payment"
              $ Just
              $ showContractError contractError
            throwError $ error $ "Failed to redeem payment: " <>
              showContractError contractError
          Right _ -> pure unit

instance ManageMarlowe m => ManageMarlowe (HalogenM state action slots msg m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName

instance ManageMarlowe m => ManageMarlowe (MaybeT m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName

instance ManageMarlowe m => ManageMarlowe (ReaderT r m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName

newtype ApplyInputError = ApplyInputError MarloweError

instance Explain ApplyInputError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain _ = text "There was an unknown problem applying an input"

instance Debuggable ApplyInputError where
  debuggable (ApplyInputError error) = encodeJson error

newtype CreateError = CreateError MarloweError

instance Explain CreateError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain _ = text "There was an unknown problem creating a contract"

instance Debuggable CreateError where
  debuggable (CreateError error) = encodeJson error
