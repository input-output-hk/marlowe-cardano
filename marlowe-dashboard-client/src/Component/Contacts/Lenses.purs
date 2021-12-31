module Component.Contacts.Lenses
  ( _addressBook
  , _cardSection
  , _walletNicknameInput
  , _addressInput
  , _remoteWalletInfo
  , _walletNickname
  , _companionAppId
  , _marloweAppId
  , _walletInfo
  , _assets
  , _previousCompanionAppState
  , _walletId
  , _pubKeyHash
  ) where

import Prologue

import Component.Contacts.Types
  ( AddressBook
  , AddressError
  , CardSection
  , State
  , WalletDetails
  , WalletId
  , WalletInfo
  , WalletNickname
  , WalletNicknameError
  )
import Component.InputField.Types (State) as InputField
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweData, MarloweParams, PubKeyHash)
import Type.Proxy (Proxy(..))
import Types (NotFoundWebData)

_addressBook :: Lens' State AddressBook
_addressBook = prop (Proxy :: _ "addressBook")

_cardSection :: Lens' State CardSection
_cardSection = prop (Proxy :: _ "cardSection")

_walletNicknameInput :: Lens' State (InputField.State WalletNicknameError)
_walletNicknameInput = prop (Proxy :: _ "walletNicknameInput")

_addressInput :: Lens' State (InputField.State AddressError)
_addressInput = prop (Proxy :: _ "addressInput")

_remoteWalletInfo :: Lens' State (NotFoundWebData WalletInfo)
_remoteWalletInfo = prop (Proxy :: _ "remoteWalletInfo")

------------------------------------------------------------
_walletNickname :: Lens' WalletDetails WalletNickname
_walletNickname = prop (Proxy :: _ "walletNickname")

_companionAppId :: Lens' WalletDetails PlutusAppId
_companionAppId = prop (Proxy :: _ "companionAppId")

_marloweAppId :: Lens' WalletDetails PlutusAppId
_marloweAppId = prop (Proxy :: _ "marloweAppId")

_walletInfo :: Lens' WalletDetails WalletInfo
_walletInfo = prop (Proxy :: _ "walletInfo")

_assets :: Lens' WalletDetails Assets
_assets = prop (Proxy :: _ "assets")

_previousCompanionAppState :: Lens' WalletDetails
  (Maybe (Map MarloweParams MarloweData))
_previousCompanionAppState = prop (Proxy :: _ "previousCompanionAppState")

------------------------------------------------------------
_walletId :: Lens' WalletInfo WalletId
_walletId = _Newtype <<< prop (Proxy :: _ "walletId")

_pubKeyHash :: Lens' WalletInfo PubKeyHash
_pubKeyHash = _Newtype <<< prop (Proxy :: _ "pubKeyHash")
