module Data.Wallet
  ( SyncStatus(..)
  , WalletDetails
  , _OutOfSync
  , _Synchronized
  , _Synchronizing
  , _address
  , _assets
  , _pubKeyHash
  , _syncStatus
  , _walletId
  , _walletNickname
  , _walletInfo
  , mkWalletDetails
  , syncStatusFromNumber
  ) where

import Prologue

import Data.Address (Address)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Show.Generic (genericShow)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Language.Marlowe.Core.V1.Semantics.Types (Assets)
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Type.Proxy (Proxy(..))

data SyncStatus
  = OutOfSync
  | Synchronizing Number
  | Synchronized

derive instance Generic SyncStatus _
derive instance Eq SyncStatus
derive instance Ord SyncStatus
instance Show SyncStatus where
  show = genericShow

_OutOfSync :: Prism' SyncStatus Unit
_OutOfSync = prism' (const OutOfSync) case _ of
  OutOfSync -> Just unit
  _ -> Nothing

_Synchronizing :: Prism' SyncStatus Number
_Synchronizing = prism' Synchronizing case _ of
  Synchronizing sync -> Just sync
  _ -> Nothing

_Synchronized :: Prism' SyncStatus Unit
_Synchronized = prism' (const Synchronized) case _ of
  Synchronized -> Just unit
  _ -> Nothing

syncStatusFromNumber :: Number -> SyncStatus
syncStatusFromNumber n
  | n < 1.0 = Synchronizing $ max 0.0 n
  | otherwise = Synchronized

------------------------------------------------------------
type WalletDetailsFields =
  { walletNickname :: WalletNickname
  , walletInfo :: WalletInfo
  , assets :: Assets
  , syncStatus :: SyncStatus
  }

newtype WalletDetails = WalletDetails WalletDetailsFields

derive instance Eq WalletDetails
derive newtype instance Show WalletDetails

mkWalletDetails
  :: WalletNickname -> WalletInfo -> WalletDetails
mkWalletDetails walletNickname walletInfo =
  WalletDetails
    { walletNickname
    , walletInfo
    , assets: mempty
    , syncStatus: OutOfSync
    }

_WalletDetails :: Iso' WalletDetails WalletDetailsFields
_WalletDetails = iso
  (\(WalletDetails details) -> details)
  (\details -> WalletDetails details)

------------------------------------------------------------
_walletNickname :: Lens' WalletDetails WalletNickname
_walletNickname = _WalletDetails <<< prop (Proxy :: _ "walletNickname")

_syncStatus :: Lens' WalletDetails SyncStatus
_syncStatus = _WalletDetails <<< prop (Proxy :: _ "syncStatus")

_assets :: Lens' WalletDetails Assets
_assets = _WalletDetails <<< prop (Proxy :: _ "assets")

_walletInfo :: Lens' WalletDetails WalletInfo
_walletInfo = _WalletDetails <<< prop (Proxy :: _ "walletInfo")

_walletId :: Lens' WalletDetails WalletId
_walletId = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "walletId")

_pubKeyHash :: Lens' WalletDetails PaymentPubKeyHash
_pubKeyHash = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "pubKeyHash")

_address :: Lens' WalletDetails Address
_address = _walletInfo <<< _Newtype <<< prop (Proxy :: _ "address")
