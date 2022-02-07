module Capability.MarloweStorage
  ( addressBookLocalStorageKey
  , class ManageMarloweStorage
  , clearAllLocalStorage
  , getContractNicknames
  , insertIntoContractNicknames
  , modifyAddressBook
  , modifyAddressBook_
  , walletRoleContractsLocalStorageKey
  , updateWallet
  , getWallet
  ) where

import Prologue

import AppM (AppM)
import Control.Monad.Except (lift)
import Data.AddressBook (AddressBook)
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.ContractNickname (ContractNickname)
import Data.Either (hush)
import Data.Map (Map, insert)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Wallet (WalletDetails, mkWalletDetails)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect.Class (liftEffect)
import Halogen (HalogenM)
import Halogen.Store.Monad (getStore, updateStore)
import LocalStorage (Key(..), getItem, removeItem, setItem)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Store (Action(..))

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
  getContractNicknames :: m (Map PlutusAppId ContractNickname)
  insertIntoContractNicknames :: PlutusAppId -> ContractNickname -> m Unit
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

  -- contract nicknames
  getContractNicknames = do
    mContractNicknamesJson <- liftEffect $ getItem
      contractNicknamesLocalStorageKey
    pure $ fromMaybe Map.empty $ hush <<< parseDecodeJson =<<
      mContractNicknamesJson
  insertIntoContractNicknames plutusAppId nickname = do
    contractNicknames <- getContractNicknames
    let
      updatedContractNicknames = insert plutusAppId nickname contractNicknames
    liftEffect $ setItem contractNicknamesLocalStorageKey $ encodeStringifyJson
      updatedContractNicknames
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
  getContractNicknames = lift getContractNicknames
  insertIntoContractNicknames plutusAppId nickname = lift $
    insertIntoContractNicknames plutusAppId nickname
  updateWallet = lift <<< updateWallet
  getWallet = lift getWallet
