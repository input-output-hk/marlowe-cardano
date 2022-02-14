module Page.Welcome.RestoreWallet.Types where

import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Wallet (WalletDetails)
import Data.WalletNickname (WalletNickname)
import Halogen as H
import Halogen.Form.Input (FieldState)
import Type.Proxy (Proxy(..))

type Input =
  {
  }

data Msg
  = CancelClicked
  | WalletRestored WalletDetails

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

type RestoreWalletParams =
  { mnemonic :: MnemonicPhrase
  , nickname :: WalletNickname
  }

type RestoreWalletFields =
  { mnemonic :: FieldState MnemonicPhrase
  , nickname :: FieldState WalletNickname
  }

_nickname = Proxy :: Proxy "nickname"
_mnemonic = Proxy :: Proxy "mnemonic"
