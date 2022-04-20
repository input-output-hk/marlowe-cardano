module Capability.Marlowe
  ( ApplyInputError(..)
  , CreateError(..)
  , RedeemError(..)
  , applyTransactionInput
  , class ManageMarlowe
  , initializeContract
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Capability.PAB (stopContract)
import Capability.PlutusApps.MarloweApp (applyInputs, createContract, redeem) as MarloweApp
import Capability.Wallet (class ManageWallet)
import Component.ContractSetup.Types (ContractParams)
import Component.Template.State
  ( InstantiateContractErrorRow
  , instantiateExtendedContract
  )
import Control.Logger.Structured (error, info, info', warning')
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Fork.Class (class MonadBracket)
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
import Effect.Aff (Aff, Error, forkAff, joinFiber)
import Effect.Aff.Class (liftAff)
import Effect.Aff.Unlift
  ( class MonadUnliftAff
  , UnliftAff
  , askUnliftAff
  , unliftAff
  )
import Errors.Debuggable (class Debuggable)
import Errors.Explain (class Explain)
import Halogen (HalogenM)
import Halogen.Store.Monad (updateStore)
import Language.Marlowe.Client (MarloweError)
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Server (Api) as MarloweApp
import Marlowe.Semantics (MarloweParams, TokenName, TransactionInput)
import Plutus.PAB.Webserver (Api) as PAB
import Servant.PureScript (class MonadAjax)
import Store as Store
import Text.Pretty (text)
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
    -> m (AjaxResponse (Aff (Either RedeemError Unit)))

withRestartOnTimeout
  :: forall f m e a b
   . MonadUnliftAff m
  => MonadBracket Error f m
  => MonadAjax PAB.Api m
  => MonadRec m
  => PlutusAppId
  -> UnliftAff (AppM m)
  -> Aff (Maybe a)
  -> e
  -> (a -> AppM m (Either e b))
  -> Aff (Either e b)
withRestartOnTimeout marloweAppId u aff timeoutError f = do
  mResult <- aff
  unliftAff u case mResult of
    Nothing -> do
      warning' "MarloweApp timed out, stoping instance."
      stopResult <- stopContract marloweAppId
      case stopResult of
        Left err -> error "Failed to stop MarloweApp" err
        Right _ -> info' "MarloweApp stopped successfully."
      pure $ Left timeoutError
    Just result -> f result

instance
  ( MonadUnliftAff m
  , MonadBracket Error f m
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
      let
        newContract =
          NewContract reqId nickname template.metaData Nothing contract
      lift $ updateStore $ Store.ContractCreated newContract

      -- Already fork and await the pending result here, so we don't have to
      -- wait for the caller to run it to update the store.
      resultFiber <- liftAff $ forkAff $ withRestartOnTimeout
        marloweAppId
        u
        awaitContractCreation
        CreateTimeout
        \mParams ->
          do
            -- Update the contract's representation in the store to use its
            -- MarloweParams if successful, or show an error otherwise.
            case mParams of
              Left contractError -> do
                updateStore $ Store.ContractStartFailed newContract
                  contractError
                pure $ Left $ CreateError contractError
              Right marloweParams -> do
                updateStore $ Store.ContractStarted newContract
                  marloweParams
                pure $ Right marloweParams

      pure $ newContract /\ joinFiber resultFiber

  -- "apply-inputs" to a Marlowe contract on the blockchain
  applyTransactionInput wallet marloweParams transactionInput = do
    info "Applying input " $ encodeJson { marloweParams, transactionInput }
    u <- askUnliftAff
    runExceptT do
      let marloweAppId = view _marloweAppId wallet
      awaitResult <- ExceptT
        $ MarloweApp.applyInputs marloweAppId marloweParams transactionInput
      pure
        $ withRestartOnTimeout marloweAppId u awaitResult ApplyInputTimeout
        -- We wrap the MarloweError in an Explainable and Debuggable context
        $ pure <<< lmap ApplyInputError

  -- "redeem" payments from a Marlowe contract on the blockchain
  redeem wallet marloweParams tokenName = do
    info "Redeeming payments" $ encodeJson { marloweParams, tokenName }
    u <- askUnliftAff
    runExceptT do
      let marloweAppId = view _marloweAppId wallet
      let address = view _address wallet
      awaitResult <- ExceptT
        $ MarloweApp.redeem marloweAppId marloweParams tokenName address
      pure
        $ withRestartOnTimeout marloweAppId u awaitResult RedeemTimeout
        -- We wrap the MarloweError in an Explainable and Debuggable context
        $ pure <<< lmap RedeemError

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

data ApplyInputError = ApplyInputTimeout | ApplyInputError MarloweError

instance Explain ApplyInputError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain ApplyInputTimeout = text
    "The Marlowe control app is not responding. restarting now."
  explain _ = text "There was an unknown problem applying an input"

instance Debuggable ApplyInputError where
  debuggable (ApplyInputError error) = encodeJson error
  debuggable ApplyInputTimeout = encodeJson "ApplyInputTimeout"

data RedeemError = RedeemTimeout | RedeemError MarloweError

instance Explain RedeemError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain RedeemTimeout = text
    "The Marlowe control app is not responding. restarting now."
  explain _ = text "There was an unknown problem redeeming a payment"

instance Debuggable RedeemError where
  debuggable (RedeemError error) = encodeJson error
  debuggable RedeemTimeout = encodeJson "RedeemTimeout"

data CreateError = CreateTimeout | CreateError MarloweError

instance Explain CreateError where
  -- TODO: SCP-3340 Convert the MarloweErrors into useful messages
  explain CreateTimeout = text
    "The Marlowe control app is not responding. restarting now."
  explain _ = text "There was an unknown problem creating a contract"

instance Debuggable CreateError where
  debuggable (CreateError error) = encodeJson error
  debuggable CreateTimeout = encodeJson "CreateTimeout"
