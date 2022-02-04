module Store
  ( Action(..)
  , Store
  , mkStore
  , reduce
  ) where

import Prologue

import Data.AddressBook (AddressBook)
import Data.ContractNickname (ContractNickname)
import Data.Lens (_Just, (.~))
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID)
import Data.Wallet (WalletDetails, _assets, _companionAppId, _marloweAppId)
import Marlowe.Execution.Types as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, Slot)
import MarloweContract (MarloweContract(..))
import Toast.Types (ToastMessage)

type Store =
  { addressBook :: AddressBook
  , currentSlot :: Slot
  , toast :: Maybe ToastMessage
  , wallet :: Maybe WalletDetails
  , newContracts :: Map UUID (ContractNickname /\ MetaData)
  , syncedContracts :: Map MarloweParams Execution.State
  -- this property shouldn't be necessary, but at the moment we are getting too many update notifications
  -- through the PAB - so until that bug is fixed, we use this to check whether an update notification
  -- really has changed anything
  , previousCompanionAppState :: Maybe (Map MarloweParams MarloweData)
  }

mkStore :: AddressBook -> Maybe WalletDetails -> Store
mkStore addressBook wallet =
  { addressBook
  , currentSlot: zero
  , toast: Nothing
  , newContracts: Map.empty
  , syncedContracts: Map.empty
  , wallet
  , previousCompanionAppState: Nothing
  }

data Action
  -- Backend Notifications
  = AdvanceToSlot Slot
  | NewCompanionAppStateObserved (Map MarloweParams MarloweData)
  -- Contract
  | AddStartingContract (UUID /\ ContractNickname /\ MetaData)
  -- Wallet
  | ModifyAddressBook (AddressBook -> AddressBook)
  | ActivateWallet WalletDetails
  | ChangePlutusScript MarloweContract PlutusAppId
  | UpdateAssets Assets
  | DeactivateWallet
  -- Toast
  | ShowToast ToastMessage
  | ClearToast

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- Backend Notifications
  -- FIXME: SCP-3208
  AdvanceToSlot newSlot -> store { currentSlot = newSlot }
  NewCompanionAppStateObserved state ->
    store { previousCompanionAppState = Just state }
  -- Contract
  AddStartingContract (reqId /\ contractNickname /\ metadata) -> store
    { newContracts = Map.insert reqId (contractNickname /\ metadata)
        store.newContracts
    }
  -- Wallet
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  ActivateWallet wallet -> store { wallet = Just wallet }
  ChangePlutusScript MarloweApp plutusAppId ->
    store { wallet = store.wallet # _Just <<< _marloweAppId .~ plutusAppId }
  ChangePlutusScript WalletCompanion plutusAppId ->
    store { wallet = store.wallet # _Just <<< _companionAppId .~ plutusAppId }
  ChangePlutusScript MarloweFollower _ -> store
  UpdateAssets assets ->
    store { wallet = store.wallet # _Just <<< _assets .~ assets }
  DeactivateWallet -> store { wallet = Nothing }
  -- Toast
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
