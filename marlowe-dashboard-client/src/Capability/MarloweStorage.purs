module Capability.MarloweStorage
  ( class ManageMarloweStorage
  , clearAllLocalStorage
  , getWalletLibrary
  , insertIntoWalletLibrary
  , getContractNicknames
  , insertIntoContractNicknames
  , getContracts
  , insertContract
  , getAllWalletRoleContracts
  , getWalletRoleContracts
  , insertWalletRoleContracts
  ) where

import Prologue
import AppM (AppM)
import Component.Contacts.Lenses (_walletNickname)
import Component.Contacts.Types (WalletDetails, WalletLibrary)
import Control.Monad.Except (lift)
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Either (hush)
import Data.Lens (view)
import Data.Map (Map, insert, lookup)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Effect.Class (liftEffect)
import Halogen (HalogenM)
import LocalStorage (Key(..), getItem, removeItem, setItem)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (MarloweData, MarloweParams, TransactionInput)

walletLibraryLocalStorageKey :: Key
walletLibraryLocalStorageKey = Key "walletLibrary"

contractNicknamesLocalStorageKey :: Key
contractNicknamesLocalStorageKey = Key "contractNicknames"

contractsLocalStorageKey :: Key
contractsLocalStorageKey = Key "walletContracts"

walletRoleContractsLocalStorageKey :: Key
walletRoleContractsLocalStorageKey = Key "walletRoleContracts"

class
  Monad m <=
  ManageMarloweStorage m where
  clearAllLocalStorage :: m Unit
  -- wallet library
  getWalletLibrary :: m WalletLibrary
  insertIntoWalletLibrary :: WalletDetails -> m Unit
  -- contract nicknames
  getContractNicknames :: m (Map PlutusAppId String)
  insertIntoContractNicknames :: PlutusAppId -> String -> m Unit
  getContracts :: m
    (Map MarloweParams (Tuple MarloweData (Array TransactionInput)))
  insertContract
    :: MarloweParams -> (Tuple MarloweData (Array TransactionInput)) -> m Unit
  getAllWalletRoleContracts :: m (Map String (Map MarloweParams MarloweData))
  getWalletRoleContracts :: String -> m (Map MarloweParams MarloweData)
  insertWalletRoleContracts :: String -> MarloweParams -> MarloweData -> m Unit

instance manageMarloweStorageAppM :: ManageMarloweStorage AppM where
  clearAllLocalStorage =
    liftEffect do
      removeItem walletLibraryLocalStorageKey
      removeItem contractNicknamesLocalStorageKey
      removeItem contractsLocalStorageKey
      removeItem walletRoleContractsLocalStorageKey
  -- wallet library
  getWalletLibrary = do
    mWalletLibraryJson <- liftEffect $ getItem walletLibraryLocalStorageKey
    pure $ fromMaybe Map.empty $ hush <<< parseDecodeJson =<< mWalletLibraryJson
  insertIntoWalletLibrary walletDetails = do
    walletLibrary <- getWalletLibrary
    let
      walletNickname = view _walletNickname walletDetails

      updatedWalletLibrary = insert walletNickname walletDetails walletLibrary
    liftEffect $ setItem walletLibraryLocalStorageKey $ encodeStringifyJson
      updatedWalletLibrary
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
  getContracts = do
    mContractsJson <- liftEffect $ getItem contractsLocalStorageKey
    pure $ fromMaybe Map.empty $ hush <<< parseDecodeJson =<< mContractsJson
  insertContract marloweParams contractData = do
    existingContracts <- getContracts
    let
      newContracts = insert marloweParams contractData existingContracts
    void $ liftEffect $ setItem contractsLocalStorageKey $ encodeStringifyJson
      newContracts
  getAllWalletRoleContracts = do
    mAllWalletRoleContracts <- liftEffect $ getItem
      walletRoleContractsLocalStorageKey
    pure $ fromMaybe Map.empty $ hush <<< parseDecodeJson =<<
      mAllWalletRoleContracts
  getWalletRoleContracts walletId = do
    allWalletRoleContracts <- getAllWalletRoleContracts
    pure $ fromMaybe Map.empty $ lookup walletId allWalletRoleContracts
  insertWalletRoleContracts walletId marloweParams marloweData = do
    allWalletRoleContracts <- getAllWalletRoleContracts
    walletRoleContracts <- getWalletRoleContracts walletId
    let
      newWalletRoleContracts = insert marloweParams marloweData
        walletRoleContracts

      newAllWalletRoleContracts = insert walletId newWalletRoleContracts
        allWalletRoleContracts
    void $ liftEffect $ setItem walletRoleContractsLocalStorageKey
      $ encodeStringifyJson newAllWalletRoleContracts

instance manageMarloweStorageHalogenM ::
  ManageMarloweStorage m =>
  ManageMarloweStorage (HalogenM state action slots msg m) where
  clearAllLocalStorage = lift clearAllLocalStorage
  getWalletLibrary = lift getWalletLibrary
  insertIntoWalletLibrary = lift <<< insertIntoWalletLibrary
  getContractNicknames = lift getContractNicknames
  insertIntoContractNicknames plutusAppId nickname = lift $
    insertIntoContractNicknames plutusAppId nickname
  getContracts = lift getContracts
  insertContract marloweParams contractData = lift $ insertContract
    marloweParams
    contractData
  getAllWalletRoleContracts = lift getAllWalletRoleContracts
  getWalletRoleContracts = lift <<< getWalletRoleContracts
  insertWalletRoleContracts walletId marloweParams marloweData = lift $
    insertWalletRoleContracts walletId marloweParams marloweData
