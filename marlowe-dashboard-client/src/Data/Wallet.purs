module Data.Wallet
  ( WalletDetails
  , _assets
  , _companionAppId
  , _marloweAppId
  , _pubKeyHash
  , _walletId
  , _walletInfo
  , _walletNickname
  , mkWalletDetails
  ) where

import Prologue

import Data.Argonaut (class DecodeJson, class EncodeJson)
import Data.Lens (Lens', lens)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Semantics (Assets)
import Type.Proxy (Proxy(..))

type WalletDetails' =
  { walletNickname :: WalletNickname
  -- FIXME-3208: Put the plutusAppId under a Connected/Disconnected
  , companionAppId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  , walletInfo :: WalletInfo
  , assets :: Assets
  }

newtype WalletDetails = WalletDetails WalletDetails'

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

_PrivateNewtype :: Lens' WalletDetails WalletDetails'
_PrivateNewtype = lens
  (\(WalletDetails details) -> details)
  (\_ details -> WalletDetails details)

------------------------------------------------------------
_walletNickname :: Lens' WalletDetails WalletNickname
_walletNickname = _PrivateNewtype <<< prop (Proxy :: _ "walletNickname")

_companionAppId :: Lens' WalletDetails PlutusAppId
_companionAppId = _PrivateNewtype <<< prop (Proxy :: _ "companionAppId")

_marloweAppId :: Lens' WalletDetails PlutusAppId
_marloweAppId = _PrivateNewtype <<< prop (Proxy :: _ "marloweAppId")

_walletInfo :: Lens' WalletDetails WalletInfo
_walletInfo = _PrivateNewtype <<< prop (Proxy :: _ "walletInfo")

_assets :: Lens' WalletDetails Assets
_assets = _PrivateNewtype <<< prop (Proxy :: _ "assets")

------------------------------------------------------------
_walletId :: Lens' WalletInfo WalletId
_walletId = _Newtype <<< prop (Proxy :: _ "walletId")

_pubKeyHash :: Lens' WalletInfo PaymentPubKeyHash
_pubKeyHash = _Newtype <<< prop (Proxy :: _ "pubKeyHash")

