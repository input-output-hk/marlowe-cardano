module Data.PABConnectedWallet
  ( PABConnectedWallet
  , _assets
  , _companionAppId
  , _marloweAppId
  , _pubKeyHash
  , _walletId
  , _walletNickname
  , connectWallet
  ) where

import Prologue

import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Wallet (WalletDetails)
import Data.Wallet as Wallet
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets)
import Type.Proxy (Proxy(..))

type PABConnectedWalletFields =
  { companionAppId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  , wallet :: WalletDetails
  }

newtype PABConnectedWallet = PABConnectedWallet PABConnectedWalletFields

derive instance Eq PABConnectedWallet

connectWallet
  :: { companionAppId :: PlutusAppId, marloweAppId :: PlutusAppId }
  -> WalletDetails
  -> PABConnectedWallet
connectWallet { companionAppId, marloweAppId } wallet = PABConnectedWallet
  { companionAppId, marloweAppId, wallet }

_PABConnectedWallet :: Lens' PABConnectedWallet PABConnectedWalletFields
_PABConnectedWallet = iso
  (\(PABConnectedWallet wallet) -> wallet)
  (\wallet -> PABConnectedWallet wallet)

_Wallet :: Lens' PABConnectedWallet WalletDetails
_Wallet = _PABConnectedWallet <<< prop (Proxy :: _ "wallet")

------------------------------------------------------------
_walletNickname :: Lens' PABConnectedWallet WalletNickname
_walletNickname = _Wallet <<< Wallet._walletNickname

_companionAppId :: Lens' PABConnectedWallet PlutusAppId
_companionAppId = _PABConnectedWallet <<< prop (Proxy :: _ "companionAppId")

_marloweAppId :: Lens' PABConnectedWallet PlutusAppId
_marloweAppId = _PABConnectedWallet <<< prop (Proxy :: _ "marloweAppId")

_assets :: Lens' PABConnectedWallet Assets
_assets = _Wallet <<< Wallet._assets

_walletId :: Lens' PABConnectedWallet WalletId
_walletId = _Wallet <<< Wallet._walletId

_pubKeyHash :: Lens' PABConnectedWallet PaymentPubKeyHash
_pubKeyHash = _Wallet <<< Wallet._pubKeyHash

