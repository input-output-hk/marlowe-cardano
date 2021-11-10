module Component.Contacts.Lenses
  ( _walletLibrary
  , _cardSection
  , _walletNicknameInput
  , _walletIdInput
  , _remoteWalletInfo
  , _walletNickname
  , _companionAppId
  , _marloweAppId
  , _walletInfo
  , _assets
  , _previousCompanionAppState
  , _wallet
  , _pubKeyHash
  ) where

import Prologue
import Component.Contacts.Types
  ( CardSection
  , State
  , Wallet
  , WalletIdError
  , WalletInfo
  , WalletLibrary
  , WalletNickname
  , WalletNicknameError
  , WalletDetails
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

_walletLibrary :: Lens' State WalletLibrary
_walletLibrary = prop (Proxy :: _ "walletLibrary")

_cardSection :: Lens' State CardSection
_cardSection = prop (Proxy :: _ "cardSection")

_walletNicknameInput :: Lens' State (InputField.State WalletNicknameError)
_walletNicknameInput = prop (Proxy :: _ "walletNicknameInput")

_walletIdInput :: Lens' State (InputField.State WalletIdError)
_walletIdInput = prop (Proxy :: _ "walletIdInput")

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
_wallet :: Lens' WalletInfo Wallet
_wallet = _Newtype <<< prop (Proxy :: _ "wallet")

_pubKeyHash :: Lens' WalletInfo PubKeyHash
_pubKeyHash = _Newtype <<< prop (Proxy :: _ "pubKeyHash")
