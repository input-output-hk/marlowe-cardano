module Store where

import Prologue

import Data.AddressBook (AddressBook)
import Data.DateTime.Instant (Instant)
import Data.Lens (Lens', (^?))
import Data.Lens.Record (prop)
import Data.LocalContractNicknames (LocalContractNicknames)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.NewContract (NewContract)
import Data.PABConnectedWallet (_assets)
import Data.Set (Set)
import Data.Wallet (WalletDetails)
import Errors.Explain (explainString)
import Language.Marlowe.Client (ContractHistory, MarloweError)
import Marlowe.Execution.Types as Execution
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Contract.V1.Types (RoleToken)
import Marlowe.Semantics (Assets(..), MarloweParams, Token)
import Store.Contracts (ContractStore, mkContractStore, tick)
import Store.Contracts as Contracts
import Store.Wallet (WalletAction, WalletStore, _connectedWallet)
import Store.Wallet as Wallet
import Toast.Types (ToastMessage, errorToast)
import Type.Proxy (Proxy(..))
import Types (JsonAjaxError)

type Store =
  { addressBook :: AddressBook
  , wallet :: WalletStore
  -- # Contracts
  , contracts :: ContractStore
  -- # Backend Notifications
  , currentTime :: Instant
  -- # System wide components
  -- This is to make sure only one dropdown at a time is open, in order to
  -- overcome a limitation of nselect that prevents it from closing the
  -- dropdown on blur.
  , openDropdown :: Maybe String
  , toast :: Maybe ToastMessage
  }

_wallet :: forall r a. Lens' { wallet :: a | r } a
_wallet = prop (Proxy :: _ "wallet")

_contracts :: forall r a. Lens' { contracts :: a | r } a
_contracts = prop (Proxy :: _ "contracts")

mkStore
  :: Instant
  -> AddressBook
  -> LocalContractNicknames
  -> Maybe WalletDetails
  -> Store
mkStore currentTime addressBook contractNicknames wallet =
  { -- # Wallet
    addressBook
  , wallet: maybe Wallet.Disconnected Wallet.Connecting wallet
  -- # Contracts
  , contracts: mkContractStore contractNicknames
  -- # Time
  , currentTime
  -- # System wide components
  , openDropdown: Nothing
  , toast: Nothing
  }

data Action
  -- Time
  = Tick Instant
  -- Contract
  | FollowerAppsActivated (Set (Tuple MarloweParams PlutusAppId))
  | ContractCreated NewContract
  | ContractHistoryUpdated PlutusAppId MetaData ContractHistory
  | ModifyContractNicknames (LocalContractNicknames -> LocalContractNicknames)
  | ModifySyncedContract MarloweParams (Execution.State -> Execution.State)
  | ContractStarted NewContract MarloweParams
  | ContractStartFailed NewContract MarloweError
  | LoadRoleTokens (Set Token)
  | LoadRoleTokenFailed Token JsonAjaxError
  | RoleTokenLoaded RoleToken
  -- Address book
  | ModifyAddressBook (AddressBook -> AddressBook)
  -- Wallet
  | Wallet WalletAction
  -- System wide components
  | Disconnect
  | ShowToast ToastMessage
  | ClearToast
  | DropdownOpened String
  | DropdownClosed

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- Time
  Tick currentTime -> case tick currentTime store.contracts of
    Left error -> reduce store
      $ ShowToast
      $ errorToast "Error updating contract state with new time"
      $ Just (explainString error)
    Right contracts -> store
      { currentTime = currentTime
      , contracts = contracts
      }
  -- Contract
  FollowerAppsActivated followers ->
    updateContractStore $ Contracts.FollowerAppsActivated followers
  ContractCreated startingContractInfo ->
    updateContractStore $ Contracts.ContractCreated startingContractInfo
  ContractHistoryUpdated followerId metadata history ->
    updateContractStore $ Contracts.ContractHistoryUpdated
      store.currentTime
      followerId
      metadata
      history
  ModifyContractNicknames f ->
    updateContractStore $ Contracts.ModifyContractNicknames f
  ModifySyncedContract marloweParams f ->
    updateContractStore $ Contracts.ModifySyncedContract marloweParams f
  ContractStarted newContract marloweParams ->
    updateContractStore $ Contracts.ContractStarted newContract marloweParams
  ContractStartFailed newContract marloweError ->
    updateContractStore $ Contracts.ContractStartFailed newContract marloweError
  LoadRoleTokens tokens ->
    updateContractStore $ Contracts.LoadRoleTokens tokens
  LoadRoleTokenFailed token ajaxError ->
    updateContractStore $ Contracts.LoadRoleTokenFailed token ajaxError
  RoleTokenLoaded roleToken ->
    updateContractStore $ Contracts.RoleTokenLoaded roleToken
  -- Address book
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  -- Wallet
  Wallet action -> store
    { wallet = Wallet.reduce store.wallet action
    , contracts =
        let
          oldAssets = fromMaybe
            (Assets Map.empty)
            (store.wallet ^? _connectedWallet <<< _assets)
        in
          case action of
            Wallet.OnAssetsChanged newAssets
              | newAssets /= oldAssets ->
                  Contracts.reduce store.contracts
                    $ Contracts.AssetsChanged newAssets
            _ -> store.contracts
    }
  -- Toast
  ShowToast msg -> store { toast = Just msg }
  ClearToast -> store { toast = Nothing }
  -- Dropdown
  DropdownOpened dropdown -> store { openDropdown = Just dropdown }
  DropdownClosed -> store { openDropdown = Nothing }
  Disconnect -> (updateContractStore Contracts.Reset)
    { wallet = Wallet.Disconnected
    }
  where
  updateContractStore action = store
    { contracts = Contracts.reduce store.contracts action
    }
