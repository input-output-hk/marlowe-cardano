module Data.Wallet
  ( WalletDetails
  , _assets
  , _companionAppId
  , _marloweAppId
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
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Semantics (Assets)
import Type.Proxy (Proxy(..))

type WalletDetailsFields =
  { walletNickname :: WalletNickname
  -- FIXME-3208: Put the plutusAppId under a Connected/Disconnected
  , companionAppId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  , walletInfo :: WalletInfo
  , assets :: Assets
  }

newtype WalletDetails = WalletDetails WalletDetailsFields

derive instance Eq WalletDetails

-- FIXME-3208: We should probably not have these instances
derive newtype instance EncodeJson WalletDetails
derive newtype instance DecodeJson WalletDetails

-- FIXME-3208: We should factor out the PlutusAppId from this constructor.
mkWalletDetails
  :: WalletNickname -> PlutusAppId -> PlutusAppId -> WalletInfo -> WalletDetails
mkWalletDetails walletNickname companionAppId marloweAppId walletInfo =
  WalletDetails
    { walletNickname
    , companionAppId
    , marloweAppId
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

_companionAppId :: Lens' WalletDetails PlutusAppId
_companionAppId = _WalletDetails <<< prop (Proxy :: _ "companionAppId")

_marloweAppId :: Lens' WalletDetails PlutusAppId
_marloweAppId = _WalletDetails <<< prop (Proxy :: _ "marloweAppId")

_assets :: Lens' WalletDetails Assets
_assets = _WalletDetails <<< prop (Proxy :: _ "assets")

_walletInfo :: Lens' WalletDetails WalletInfo
_walletInfo = _WalletDetails <<< prop (Proxy :: _ "walletInfo")

_walletId :: Lens' WalletDetails WalletId
_walletId = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "walletId")

_pubKeyHash :: Lens' WalletDetails PaymentPubKeyHash
_pubKeyHash = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "pubKeyHash")

