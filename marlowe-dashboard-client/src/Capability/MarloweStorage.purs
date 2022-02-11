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
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Either (hush)
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
import Halogen (HalogenM)
import Halogen.Store.Monad (getStore, updateStore)
import LocalStorage (Key(..), getItem, removeItem, setItem)
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

class
  Monad m <=
  ManageMarloweStorage m where
  clearAllLocalStorage :: m Unit
  -- Address book
  modifyAddressBook :: (AddressBook -> AddressBook) -> m AddressBook
  -- contract nicknames
  modifyContractNicknames
    :: (LocalContractNicknames -> LocalContractNicknames)
    -> m LocalContractNicknames
  -- Wallet
  updateWallet
    :: Maybe (WalletNickname /\ WalletId /\ PaymentPubKeyHash) -> m Unit
  getWallet :: m (Maybe WalletDetails)

modifyAddressBook_
  :: forall m
   . ManageMarloweStorage m
  => (AddressBook -> AddressBook)
  -> m Unit
modifyAddressBook_ = void <<< modifyAddressBook

getAddressBook :: Effect AddressBook
getAddressBook = decodeAddressBook <$> getItem addressBookLocalStorageKey
  where
  decodeAddressBook mAddressBookJson = fromMaybe AddressBook.empty $
    hush <<< parseDecodeJson =<< mAddressBookJson

getContractNicknames :: Effect LocalContractNicknames
getContractNicknames = decodeContractNicknames <$> getItem
  contractNicknamesLocalStorageKey
  where
  decodeContractNicknames mContractNicknames =
    fromMaybe emptyLocalContractNicknames $ hush <<< parseDecodeJson =<<
      mContractNicknames

instance manageMarloweStorageAppM :: ManageMarloweStorage AppM where
  clearAllLocalStorage =
    liftEffect do
      removeItem addressBookLocalStorageKey
      removeItem contractNicknamesLocalStorageKey
      removeItem walletRoleContractsLocalStorageKey
  modifyAddressBook f = do
    updateStore $ ModifyAddressBook f
    addressBook <- _.addressBook <$> getStore
    liftEffect
      $ setItem addressBookLocalStorageKey
      $ encodeStringifyJson addressBook
    pure addressBook

  modifyContractNicknames f = do
    updateStore $ ModifyContractNicknames f
    contractNicknames <- ContractStore.getContractNicknames <<< _.contracts <$>
      getStore
    liftEffect
      $ setItem contractNicknamesLocalStorageKey
      $ encodeStringifyJson contractNicknames
    pure contractNicknames

  -- Wallet
  updateWallet Nothing = liftEffect $ removeItem walletLocalStorageKey
  updateWallet (Just wallet) = liftEffect $ setItem walletLocalStorageKey $
    encodeStringifyJson
      wallet
  getWallet = do
    mWalletJson <- liftEffect $ getItem
      walletLocalStorageKey
    pure do
      walletJson <- mWalletJson
      walletNickName /\ walletId /\ pubKeyHash <- hush $ parseDecodeJson
        walletJson
      let
        walletInfo = WalletInfo { walletId, pubKeyHash }
      pure $ mkWalletDetails walletNickName walletInfo

instance manageMarloweStorageHalogenM ::
  ManageMarloweStorage m =>
  ManageMarloweStorage (HalogenM state action slots msg m) where
  clearAllLocalStorage = lift clearAllLocalStorage
  modifyAddressBook = lift <<< modifyAddressBook
  modifyContractNicknames = lift <<< modifyContractNicknames
  updateWallet = lift <<< updateWallet
  getWallet = lift getWallet
