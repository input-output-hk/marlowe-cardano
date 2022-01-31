module Store
  ( Action(..)
  , Store
  , reduce
  ) where

import Prologue

import Component.Contacts.Lenses (_assets)
import Component.Contacts.Types (WalletDetails)
import Data.AddressBook (AddressBook)
import Data.Lens (_Just, (.~))
import Data.Map (Map)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, Slot)
import Toast.Types (ToastMessage)

type Store =
  { addressBook :: AddressBook
  , currentSlot :: Slot
  , toast :: Maybe ToastMessage
  , wallet :: Maybe WalletDetails
  -- this property shouldn't be necessary, but at the moment we are getting too many update notifications
  -- through the PAB - so until that bug is fixed, we use this to check whether an update notification
  -- really has changed anything
  , previousCompanionAppState :: Maybe (Map MarloweParams MarloweData)
  }

data Action
  = AdvanceToSlot Slot
  | ShowToast ToastMessage
  | ModifyAddressBook (AddressBook -> AddressBook)
  | ActivateWallet WalletDetails
  | NewCompanionAppStateObserved (Map MarloweParams MarloweData)
  | UpdateAssets Assets
  | DeactivateWallet
  | ClearToast

reduce :: Store -> Action -> Store
-- TODO: currently we are only setting the currentSlot global variable, but once we
--       refactor contract state to live under the halogen store (SCP-3208) we can also move the
--       logic of AdvanceTimedoutSteps here.
reduce store = case _ of
  AdvanceToSlot newSlot -> store { currentSlot = newSlot }
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  ActivateWallet wallet -> store { wallet = Just wallet }
  UpdateAssets assets ->
    store { wallet = store.wallet # _Just <<< _assets .~ assets }
  DeactivateWallet -> store { wallet = Nothing }
  NewCompanionAppStateObserved state ->
    store { previousCompanionAppState = Just state }
