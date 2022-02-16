module Store.Wallet where

import Prologue

import Data.Lens (Prism', prism', (.~), (^.))
import Data.Lens.AffineTraversal (AffineTraversal', affineTraversal)
import Data.Map (Map)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.PABConnectedWallet as C
import Data.Tuple (uncurry)
import Data.Wallet (SyncStatus, WalletDetails)
import Data.Wallet as D
import Data.WalletId (WalletId)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics (Assets, MarloweParams)
import MarloweContract (MarloweContract(..))
import Page.Contract.Types as Contract

data WalletStore
  = Disconnected
  | Disconnecting PABConnectedWallet (Map MarloweParams Contract.State)
  | Connecting WalletDetails
  | Connected PABConnectedWallet

derive instance Eq WalletStore

_Disconnecting :: Prism'
  WalletStore
  (Tuple PABConnectedWallet (Map MarloweParams Contract.State))
_Disconnecting = prism' (uncurry Disconnecting) case _ of
  Disconnecting wallet contracts -> Just $ Tuple wallet contracts
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
    Disconnecting _ contracts -> Disconnecting wallet contracts
    s -> s
  pre = case _ of
    Connecting details -> Left $ Connecting details
    Connected wallet -> Right $ wallet
    Disconnecting wallet _ -> Right $ wallet
    Disconnected -> Left Disconnected

_walletId :: AffineTraversal' WalletStore WalletId
_walletId = affineTraversal set pre
  where
  set store walletId = case store of
    Connecting details -> Connecting $ details # D._walletId .~ walletId
    Connected wallet -> Connected $ wallet # C._walletId .~ walletId
    Disconnecting wallet contracts -> Disconnecting
      (wallet # C._walletId .~ walletId)
      contracts
    Disconnected -> Disconnected
  pre = case _ of
    Connecting details -> Right $ details ^. D._walletId
    Connected wallet -> Right $ wallet ^. C._walletId
    Disconnecting wallet _ -> Right $ wallet ^. C._walletId
    Disconnected -> Left Disconnected

_assets :: AffineTraversal' WalletStore Assets
_assets = affineTraversal set pre
  where
  set store assets = case store of
    Connecting details -> Connecting $ details # D._assets .~ assets
    Connected wallet -> Connected $ wallet # C._assets .~ assets
    Disconnecting wallet contracts -> Disconnecting
      (wallet # C._assets .~ assets)
      contracts
    Disconnected -> Disconnected
  pre = case _ of
    Connecting details -> Right $ details ^. D._assets
    Connected wallet -> Right $ wallet ^. C._assets
    Disconnecting wallet _ -> Right $ wallet ^. C._assets
    Disconnected -> Left Disconnected

_syncStatus :: AffineTraversal' WalletStore SyncStatus
_syncStatus = affineTraversal set pre
  where
  set store syncStatus = case store of
    Connecting details -> Connecting $ details # D._syncStatus .~ syncStatus
    Connected wallet -> Connected $ wallet # C._syncStatus .~ syncStatus
    Disconnecting wallet contracts -> Disconnecting
      (wallet # C._syncStatus .~ syncStatus)
      contracts
    Disconnected -> Disconnected
  pre = case _ of
    Connecting details -> Right $ details ^. D._syncStatus
    Connected wallet -> Right $ wallet ^. C._syncStatus
    Disconnecting wallet _ -> Right $ wallet ^. C._syncStatus
    Disconnected -> Left Disconnected

data WalletAction
  = OnDisconnected
  | OnDisconnect PABConnectedWallet (Map MarloweParams Contract.State)
  | OnConnect WalletDetails
  | OnConnected PABConnectedWallet
  | OnPlutusScriptChanged MarloweContract PlutusAppId
  | OnAssetsChanged Assets
  | OnSyncStatusChanged SyncStatus

reduce :: WalletStore -> WalletAction -> WalletStore
reduce store = case _ of
  OnDisconnect wallet contracts -> Disconnecting wallet contracts
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
