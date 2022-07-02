module Store where

import Prologue

import Component.Toast.Types (errorToast)
import Data.AddressBook (AddressBook)
import Data.ContractNickname (ContractNickname)
import Data.ContractStatus (ContractStatusId)
import Data.DateTime.Instant (Instant)
import Data.Lens (Lens', (^?))
import Data.Lens.Record (prop)
import Data.LocalContractNicknames (LocalContractNicknames)
import Data.Map as Map
import Data.Maybe (fromMaybe, maybe)
import Data.NewContract (NewContract)
import Data.PABConnectedWallet (_assets)
import Data.Set (Set)
import Data.Slot (Slot, zeroSlot)
import Data.Wallet (WalletDetails)
import Errors.Explain (explain)
import Language.Marlowe.Client (ContractHistory, MarloweError, UnspentPayouts)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Assets(..)
  , MarloweParams
  , Token
  )
import Language.Marlowe.Extended.V1.Metadata (MetaData)
import Marlowe.Execution.Types as Execution
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Contract.V1.Types (RoleToken)
import Store.Contracts (ContractStore, mkContractStore, tick)
import Store.Contracts as Contracts
import Store.RoleTokens
  ( RoleTokenStore
  , loadRoleTokenFailed
  , loadRoleTokens
  , mkRoleTokenStore
  , newPayoutsReceived
  , roleTokenLoaded
  , updateMyRoleTokens
  )
import Store.Toast (ToastAction, ToastStore, emptyToastStore)
import Store.Toast as Toast
import Store.Wallet (WalletAction, WalletStore, _connectedWallet)
import Store.Wallet as Wallet
import Type.Proxy (Proxy(..))
import Types (JsonAjaxError)

type Store =
  { addressBook :: AddressBook
  , wallet :: WalletStore
  , contracts :: ContractStore
  , currentTime :: Instant
  , roleTokens :: RoleTokenStore
  -- # System wide components
  -- This is to make sure only one dropdown at a time is open, in order to
  -- overcome a limitation of nselect that prevents it from closing the
  -- dropdown on blur.
  , openDropdown :: Maybe String
  , toast :: ToastStore
  -- | The slot of the current tip of the Cardano Node. Note that this
  -- | generally lags behind the true "current slot" - i.e. the slot that a
  -- | block produced this instant would have. This refers instead to the slot
  -- | of the last block produced by the node.
  , tipSlot :: Slot
  , currentSlot :: Slot
  }

type StoreLens a = Lens' Store a

_wallet :: forall r a. Lens' { wallet :: a | r } a
_wallet = prop (Proxy :: _ "wallet")

_contracts :: forall r a. Lens' { contracts :: a | r } a
_contracts = prop (Proxy :: _ "contracts")

_roleTokens :: StoreLens RoleTokenStore
_roleTokens = prop (Proxy :: _ "roleTokens")

mkStore
  :: Instant
  -> AddressBook
  -> LocalContractNicknames
  -> Maybe WalletDetails
  -> Store
mkStore currentTime addressBook contractNicknames wallet =
  { addressBook
  , wallet: maybe Wallet.Disconnected Wallet.Connecting wallet
  , contracts: mkContractStore contractNicknames
  , currentTime
  , roleTokens: mkRoleTokenStore
  -- # System wide components
  , openDropdown: Nothing
  , toast: emptyToastStore
  , tipSlot: zeroSlot
  , currentSlot: zeroSlot
  }

data Action
  -- Time
  = Tick Instant
  | SlotChanged { current :: Slot, tip :: Slot }
  -- Contract
  | FollowerAppsActivated (Set (Tuple MarloweParams PlutusAppId))
  | FollowerAppClosed PlutusAppId
  | ContractCreated NewContract
  | ContractHistoryUpdated PlutusAppId MetaData ContractHistory
  | ContractNicknameUpdated ContractStatusId ContractNickname
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
  -- Role Tokens
  | NewPayoutsReceived MarloweParams UnspentPayouts
  -- System wide components
  | Disconnect
  | Toast ToastAction
  | DropdownOpened String
  | DropdownClosed

reduce :: Store -> Action -> Store
reduce store = case _ of
  -- Time
  SlotChanged { current, tip } ->
    store { tipSlot = tip, currentSlot = current }
  Tick currentTime -> case tick currentTime store.contracts of
    Left error -> reduce store
      $ Toast
      $ Toast.Show
      $ errorToast "Error updating contract state with new time"
      $ Just (explain error)
    Right contracts -> store
      { currentTime = currentTime
      , contracts = contracts
      }
  -- Contract
  FollowerAppsActivated followers ->
    updateContractStore $ Contracts.FollowerAppsActivated followers
  FollowerAppClosed appId ->
    updateContractStore $ Contracts.FollowerAppClosed appId
  ContractCreated startingContractInfo ->
    updateContractStore $ Contracts.ContractCreated startingContractInfo
  ContractHistoryUpdated followerId metadata history ->
    updateContractStore $ Contracts.ContractHistoryUpdated
      store.currentTime
      followerId
      metadata
      history
  ContractNicknameUpdated id nickname ->
    updateContractStore $ Contracts.ContractNicknameUpdated id nickname
  ModifySyncedContract marloweParams f ->
    updateContractStore $ Contracts.ModifySyncedContract marloweParams f
  ContractStarted newContract marloweParams ->
    updateContractStore $ Contracts.ContractStarted newContract marloweParams
  ContractStartFailed newContract marloweError ->
    updateContractStore $ Contracts.ContractStartFailed newContract marloweError
  LoadRoleTokens tokens ->
    updateRoleTokenStore $ loadRoleTokens tokens
  LoadRoleTokenFailed token ajaxError ->
    updateRoleTokenStore $ loadRoleTokenFailed token ajaxError
  RoleTokenLoaded roleToken ->
    updateRoleTokenStore $ roleTokenLoaded store.addressBook roleToken
  -- Address book
  ModifyAddressBook f -> store { addressBook = f store.addressBook }
  -- Wallet
  Wallet action -> store
    { wallet = Wallet.reduce store.wallet action
    , roleTokens =
        let
          oldAssets = fromMaybe
            (Assets Map.empty)
            (store.wallet ^? _connectedWallet <<< _assets)
        in
          case action of
            Wallet.OnAssetsChanged newAssets
              | newAssets /= oldAssets ->
                  updateMyRoleTokens newAssets store.roleTokens
            _ -> store.roleTokens
    }
  -- Toast
  Toast action -> store { toast = Toast.reduce store.toast action }
  -- Dropdown
  DropdownOpened dropdown -> store { openDropdown = Just dropdown }
  DropdownClosed -> store { openDropdown = Nothing }
  Disconnect -> (updateContractStore Contracts.Reset)
    { wallet = Wallet.Disconnected
    }
  -- Role Tokens
  NewPayoutsReceived marloweParams unspentPayouts ->
    updateRoleTokenStore $ newPayoutsReceived marloweParams unspentPayouts
  where
  updateRoleTokenStore f =
    store { roleTokens = f store.roleTokens }
  updateContractStore action =
    store { contracts = Contracts.reduce store.contracts action }
