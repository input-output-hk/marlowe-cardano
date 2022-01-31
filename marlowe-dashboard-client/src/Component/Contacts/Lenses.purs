module Component.Contacts.Lenses
  ( _cardSection
  , _walletNickname
  , _companionAppId
  , _marloweAppId
  , _walletInfo
  , _assets
  , _walletId
  , _pubKeyHash
  ) where

import Prologue

import Component.Contacts.Types (CardSection, State, WalletDetails)
import Data.Lens (Lens')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Semantics (Assets)
import Type.Proxy (Proxy(..))

_cardSection :: Lens' State CardSection
_cardSection = prop (Proxy :: _ "cardSection")

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

------------------------------------------------------------
_walletId :: Lens' WalletInfo WalletId
_walletId = _Newtype <<< prop (Proxy :: _ "walletId")

_pubKeyHash :: Lens' WalletInfo PaymentPubKeyHash
_pubKeyHash = _Newtype <<< prop (Proxy :: _ "pubKeyHash")
