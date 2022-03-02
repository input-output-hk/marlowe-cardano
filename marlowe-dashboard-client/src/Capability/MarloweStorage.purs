module Capability.MarloweStorage
  ( class ManageMarloweStorage
  , clearAllLocalStorage
  , getAddressBook
  , getContractNicknames
  , getWallet
  , modifyAddressBook
  , modifyAddressBook_
  , modifyContractNicknames
  , updateWallet
  , walletRoleContractsLocalStorageKey
  ) where

import Prologue

import AppM (AppM)
import Control.Monad.Except (lift)
import Control.Monad.Maybe.Trans (MaybeT)
import Control.Monad.Reader (asks)
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Either (hush)
import Data.Lens (view)
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  )
import Data.Maybe (fromMaybe)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Wallet (WalletDetails, mkWalletDetails)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect (Effect)
import Effect.Class (liftEffect)
import Env (_localStorage)
import Halogen (HalogenM)
import Halogen.Store.Monad (getStore, updateStore)
import LocalStorage (Key(..))
import LocalStorage as LS
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Store (Action(..))
import Store.Contracts as ContractStore

addressBookLocalStorageKey :: Key
addressBookLocalStorageKey = Key "addressBook"

walletLocalStorageKey :: Key
walletLocalStorageKey = Key "wallet"

contractNicknamesLocalStorageKey :: Key
contractNicknamesLocalStorageKey = Key "contractNicknames"

walletRoleContractsLocalStorageKey :: Key
walletRoleContractsLocalStorageKey = Key "walletRoleContracts"

class Monad m <= ManageMarloweStorage m where
  clearAllLocalStorage :: m Unit
  -- Address book
  modifyAddressBook :: (AddressBook -> AddressBook) -> m AddressBook
  -- contract nicknames
  modifyContractNicknames
    :: (LocalContractNicknames -> LocalContractNicknames)
    -> m LocalContractNicknames
  -- Wallet
  updateWallet
    :: Maybe (WalletNickname /\ WalletId /\ PaymentPubKeyHash /\ Address)
    -> m Unit
  getWallet :: m (Maybe WalletDetails)

modifyAddressBook_
  :: forall m
   . ManageMarloweStorage m
  => (AddressBook -> AddressBook)
  -> m Unit
modifyAddressBook_ = void <<< modifyAddressBook

getAddressBook :: Effect AddressBook
getAddressBook = decodeAddressBook <$> LS.getItem addressBookLocalStorageKey
  where
  decodeAddressBook mAddressBookJson = fromMaybe AddressBook.empty $
    hush <<< parseDecodeJson =<< mAddressBookJson

getContractNicknames :: Effect LocalContractNicknames
getContractNicknames = decodeContractNicknames <$> LS.getItem
  contractNicknamesLocalStorageKey
  where
  decodeContractNicknames mContractNicknames =
    fromMaybe emptyLocalContractNicknames $ hush <<< parseDecodeJson =<<
      mContractNicknames

instance ManageMarloweStorage AppM where
  clearAllLocalStorage = do
    { removeItem } <- asks $ view _localStorage
    liftEffect do
      removeItem addressBookLocalStorageKey
      removeItem contractNicknamesLocalStorageKey
      removeItem walletRoleContractsLocalStorageKey

  modifyAddressBook f = do
    { setItem } <- asks $ view _localStorage
    updateStore $ ModifyAddressBook f
    addressBook <- _.addressBook <$> getStore
    liftEffect
      $ setItem addressBookLocalStorageKey
      $ encodeStringifyJson addressBook
    pure addressBook

  modifyContractNicknames f = do
    { setItem } <- asks $ view _localStorage
    updateStore $ ModifyContractNicknames f
    contractNicknames <- ContractStore.getContractNicknames <<< _.contracts <$>
      getStore
    liftEffect
      $ setItem contractNicknamesLocalStorageKey
      $ encodeStringifyJson contractNicknames
    pure contractNicknames

  -- Wallet
  updateWallet Nothing = do
    { removeItem } <- asks $ view _localStorage
    liftEffect $ removeItem walletLocalStorageKey

  updateWallet (Just wallet) = do
    { setItem } <- asks $ view _localStorage
    liftEffect $ setItem walletLocalStorageKey $ encodeStringifyJson wallet

  getWallet = do
    { getItem } <- asks $ view _localStorage
    mWalletJson <- liftEffect $ getItem
      walletLocalStorageKey
    pure do
      walletJson <- mWalletJson
      walletNickName /\ walletId /\ pubKeyHash /\ address <-
        hush $ parseDecodeJson walletJson
      let
        walletInfo = WalletInfo { walletId, pubKeyHash, address }
      pure $ mkWalletDetails walletNickName walletInfo

instance
  ManageMarloweStorage m =>
  ManageMarloweStorage (HalogenM state action slots msg m) where
  clearAllLocalStorage = lift clearAllLocalStorage
  modifyAddressBook = lift <<< modifyAddressBook
  modifyContractNicknames = lift <<< modifyContractNicknames
  updateWallet = lift <<< updateWallet
  getWallet = lift getWallet

instance ManageMarloweStorage m => ManageMarloweStorage (MaybeT m) where
  clearAllLocalStorage = lift clearAllLocalStorage
  modifyAddressBook = lift <<< modifyAddressBook
  modifyContractNicknames = lift <<< modifyContractNicknames
  updateWallet = lift <<< updateWallet
  getWallet = lift getWallet
