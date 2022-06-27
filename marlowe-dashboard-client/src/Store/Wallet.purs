module Store.Wallet where

import Prologue

import Data.Lens (Prism', prism', (.~), (^.))
import Data.Lens.AffineTraversal (AffineTraversal', affineTraversal)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.PABConnectedWallet as C
import Data.Wallet (SyncStatus, WalletDetails)
import Data.Wallet as D
import Data.WalletId (WalletId)
import Language.Marlowe.Core.V1.Semantics.Types (Assets)
import Marlowe.PAB (PlutusAppId)
import MarloweContract (MarloweContract(..))

data WalletStore
  = Disconnected
  | Disconnecting PABConnectedWallet
  | Connecting WalletDetails
  | Connected PABConnectedWallet

derive instance Eq WalletStore

_Disconnecting :: Prism' WalletStore PABConnectedWallet
_Disconnecting = prism' Disconnecting case _ of
  Disconnecting wallet -> Just wallet
  _ -> Nothing

_Connecting :: Prism' WalletStore WalletDetails
_Connecting = prism' Connecting case _ of
  Connecting details -> Just details
  _ -> Nothing

_Connected :: Prism' WalletStore PABConnectedWallet
_Connected = prism' Connected case _ of
  Connected wallet -> Just wallet
  _ -> Nothing

_connectedWallet :: AffineTraversal' WalletStore PABConnectedWallet
_connectedWallet = affineTraversal set pre
  where
  set store wallet = case store of
    Connected _ -> Connected wallet
    Disconnecting _ -> Disconnecting wallet
    s -> s
  pre = case _ of
    Connecting details -> Left $ Connecting details
    Connected wallet -> Right $ wallet
    Disconnecting wallet -> Right $ wallet
    Disconnected -> Left Disconnected

_walletId :: AffineTraversal' WalletStore WalletId
_walletId = affineTraversal set pre
  where
  set store walletId = case store of
    Connecting details -> Connecting $ details # D._walletId .~ walletId
    Connected wallet -> Connected $ wallet # C._walletId .~ walletId
    Disconnecting wallet -> Disconnecting
      (wallet # C._walletId .~ walletId)
    Disconnected -> Disconnected
  pre = case _ of
    Connecting details -> Right $ details ^. D._walletId
    Connected wallet -> Right $ wallet ^. C._walletId
    Disconnecting wallet -> Right $ wallet ^. C._walletId
    Disconnected -> Left Disconnected

_assets :: AffineTraversal' WalletStore Assets
_assets = affineTraversal set pre
  where
  set store assets = case store of
    Connecting details -> Connecting $ details # D._assets .~ assets
    Connected wallet -> Connected $ wallet # C._assets .~ assets
    Disconnecting wallet -> Disconnecting (wallet # C._assets .~ assets)
    Disconnected -> Disconnected
  pre = case _ of
    Connecting details -> Right $ details ^. D._assets
    Connected wallet -> Right $ wallet ^. C._assets
    Disconnecting wallet -> Right $ wallet ^. C._assets
    Disconnected -> Left Disconnected

_syncStatus :: AffineTraversal' WalletStore SyncStatus
_syncStatus = affineTraversal set pre
  where
  set store syncStatus = case store of
    Connecting details -> Connecting $ details # D._syncStatus .~ syncStatus
    Connected wallet -> Connected $ wallet # C._syncStatus .~ syncStatus
    Disconnecting wallet -> Disconnecting (wallet # C._syncStatus .~ syncStatus)
    Disconnected -> Disconnected
  pre = case _ of
    Connecting details -> Right $ details ^. D._syncStatus
    Connected wallet -> Right $ wallet ^. C._syncStatus
    Disconnecting wallet -> Right $ wallet ^. C._syncStatus
    Disconnected -> Left Disconnected

data WalletAction
  = OnDisconnected
  | OnDisconnect PABConnectedWallet
  | OnConnect WalletDetails
  | OnConnected PABConnectedWallet
  | OnPlutusScriptChanged MarloweContract PlutusAppId
  | OnAssetsChanged Assets
  | OnSyncStatusChanged SyncStatus

reduce :: WalletStore -> WalletAction -> WalletStore
reduce store = case _ of
  OnDisconnect wallet -> Disconnecting wallet
  OnDisconnected -> Disconnected
  OnConnect details -> Connecting details
  OnConnected wallet -> Connected wallet
  OnPlutusScriptChanged MarloweApp plutusAppId ->
    store # _Connected <<< C._marloweAppId .~ plutusAppId
  OnPlutusScriptChanged WalletCompanion plutusAppId ->
    store # _Connected <<< C._companionAppId .~ plutusAppId
  OnPlutusScriptChanged MarloweFollower _ -> store
  OnAssetsChanged assets ->
    store # _assets .~ assets
  OnSyncStatusChanged syncStatus ->
    store # _syncStatus .~ syncStatus
