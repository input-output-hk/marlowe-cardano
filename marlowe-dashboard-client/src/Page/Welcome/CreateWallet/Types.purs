module Page.Welcome.CreateWallet.Types where

import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Wallet (WalletDetails)
import Halogen as H
import Type.Proxy (Proxy(..))

type Input = {}

type NewWalletDetails =
  { mnemonic :: MnemonicPhrase
  , walletDetails :: WalletDetails
  }

data Msg
  = CancelClicked
  | WalletCreated NewWalletDetails

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

_nickname = Proxy :: Proxy "nickname"
