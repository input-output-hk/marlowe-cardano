module Data.PABConnectedWallet
  ( PABConnectedWallet
  , _assets
  , _companionAppId
  , _initialFollowers
  , _marloweAppId
  , _pubKeyHash
  , _address
  , _syncStatus
  , _walletId
  , _walletDetails
  , _walletNickname
  , connectWallet
  ) where

import Prologue

import Data.Address (Address)
import Data.Lens (Lens', iso)
import Data.Lens.Record (prop)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Set (Set)
import Data.Wallet (SyncStatus, WalletDetails)
import Data.Wallet as Wallet
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Language.Marlowe.Core.V1.Semantics.Types (Assets, MarloweParams)
import Marlowe.PAB (PlutusAppId)
import Type.Proxy (Proxy(..))

type PABConnectedWalletFields =
  { companionAppId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  , initialFollowers :: Set (Tuple MarloweParams PlutusAppId)
  , wallet :: WalletDetails
  }

newtype PABConnectedWallet = PABConnectedWallet PABConnectedWalletFields

derive instance Eq PABConnectedWallet

connectWallet
  :: { companionAppId :: PlutusAppId
     , marloweAppId :: PlutusAppId
     , initialFollowers :: Set (Tuple MarloweParams PlutusAppId)
     }
  -> WalletDetails
  -> PABConnectedWallet
connectWallet { companionAppId, marloweAppId, initialFollowers } wallet =
  PABConnectedWallet
    { companionAppId, marloweAppId, wallet, initialFollowers }

_PABConnectedWallet :: Lens' PABConnectedWallet PABConnectedWalletFields
_PABConnectedWallet = iso
  (\(PABConnectedWallet wallet) -> wallet)
  (\wallet -> PABConnectedWallet wallet)

_Wallet :: Lens' PABConnectedWallet WalletDetails
_Wallet = _PABConnectedWallet <<< prop (Proxy :: _ "wallet")

------------------------------------------------------------
_walletNickname :: Lens' PABConnectedWallet WalletNickname
_walletNickname = _Wallet <<< Wallet._walletNickname

_initialFollowers
  :: Lens' PABConnectedWallet (Set (Tuple MarloweParams PlutusAppId))
_initialFollowers = _PABConnectedWallet <<< prop (Proxy :: _ "initialFollowers")

_companionAppId :: Lens' PABConnectedWallet PlutusAppId
_companionAppId = _PABConnectedWallet <<< prop (Proxy :: _ "companionAppId")

_marloweAppId :: Lens' PABConnectedWallet PlutusAppId
_marloweAppId = _PABConnectedWallet <<< prop (Proxy :: _ "marloweAppId")

_syncStatus :: Lens' PABConnectedWallet SyncStatus
_syncStatus = _Wallet <<< Wallet._syncStatus

_assets :: Lens' PABConnectedWallet Assets
_assets = _Wallet <<< Wallet._assets

_walletId :: Lens' PABConnectedWallet WalletId
_walletId = _Wallet <<< Wallet._walletId

_pubKeyHash :: Lens' PABConnectedWallet PaymentPubKeyHash
_pubKeyHash = _Wallet <<< Wallet._pubKeyHash

_address :: Lens' PABConnectedWallet Address
_address = _Wallet <<< Wallet._address

_walletDetails :: Lens' PABConnectedWallet WalletDetails
_walletDetails = _Wallet
