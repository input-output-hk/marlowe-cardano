module Data.Wallet
  ( WalletDetails
  , _assets
  , _pubKeyHash
  , _walletId
  , _walletNickname
  , mkWalletDetails
  ) where

import Prologue

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Lens (Lens', iso)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Semantics (Assets)
import Type.Proxy (Proxy(..))

type WalletDetailsFields =
  { walletNickname :: WalletNickname
  , walletInfo :: WalletInfo
  , assets :: Assets
  }

newtype WalletDetails = WalletDetails WalletDetailsFields

derive instance Eq WalletDetails

-- FIXME-3208: We should probably not have these instances
derive newtype instance EncodeJson WalletDetails
derive newtype instance DecodeJson WalletDetails

mkWalletDetails
  :: WalletNickname -> WalletInfo -> WalletDetails
mkWalletDetails walletNickname walletInfo =
  WalletDetails
    { walletNickname
    , walletInfo
    , assets: mempty
    }

_WalletDetails :: Lens' WalletDetails WalletDetailsFields
_WalletDetails = iso
  (\(WalletDetails details) -> details)
  (\details -> WalletDetails details)

------------------------------------------------------------
_walletNickname :: Lens' WalletDetails WalletNickname
_walletNickname = _WalletDetails <<< prop (Proxy :: _ "walletNickname")

_assets :: Lens' WalletDetails Assets
_assets = _WalletDetails <<< prop (Proxy :: _ "assets")

_walletInfo :: Lens' WalletDetails WalletInfo
_walletInfo = _WalletDetails <<< prop (Proxy :: _ "walletInfo")

_walletId :: Lens' WalletDetails WalletId
_walletId = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "walletId")

_pubKeyHash :: Lens' WalletDetails PaymentPubKeyHash
_pubKeyHash = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "pubKeyHash")

