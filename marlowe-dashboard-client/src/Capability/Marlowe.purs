module Capability.Marlowe
  ( class ManageMarlowe
  , initializeContract
  , applyTransactionInput
  , redeem
  , getRoleContracts
  ) where

import Prologue

import AppM (AppM)
import Capability.PAB (class ManagePAB)
import Capability.PAB (getContractInstanceObservableState) as PAB
import Capability.PlutusApps.MarloweApp as MarloweApp
import Capability.Wallet (class ManageWallet)
import Component.ContractSetup.Types (ContractParams)
import Component.Template.State
  ( InstantiateContractErrorRow
  , instantiateExtendedContract
  )
import Control.Monad.Except (ExceptT(..), except, lift, runExceptT, withExceptT)
import Control.Monad.Maybe.Trans (MaybeT)
import Data.Argonaut.Decode (decodeJson)
import Data.Bifunctor (lmap)
import Data.DateTime.Instant (Instant)
import Data.Lens (view)
import Data.Map (Map)
import Data.NewContract (NewContract(..))
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _address
  , _companionAppId
  , _marloweAppId
  )
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Generic (class Constructors, mkConstructors')
import Effect.Aff (Aff)
import Halogen (HalogenM)
import Halogen.Store.Monad (updateStore)
import Marlowe.Extended.Metadata (ContractTemplate)
import Marlowe.Semantics
  ( MarloweData
  , MarloweParams
  , TokenName
  , TransactionInput
  )
import Store as Store
import Type.Proxy (Proxy(..))
import Type.Row (type (+))
import Types (AjaxResponse, DecodedAjaxResponse, JsonAjaxErrorRow)

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
  , ManageWallet m
  ) <=
  ManageMarlowe m where
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

instance manageMarloweAppM :: ManageMarlowe AppM where

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

instance ManageMarlowe m => ManageMarlowe (HalogenM state action slots msg m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName
  getRoleContracts = lift <<< getRoleContracts

instance ManageMarlowe m => ManageMarlowe (MaybeT m) where
  initializeContract currentInstant template params wallet =
    lift $ initializeContract currentInstant template params wallet
  applyTransactionInput walletDetails marloweParams transactionInput =
    lift $ applyTransactionInput walletDetails marloweParams transactionInput
  redeem walletDetails marloweParams tokenName =
    lift $ redeem walletDetails marloweParams tokenName
  getRoleContracts = lift <<< getRoleContracts
