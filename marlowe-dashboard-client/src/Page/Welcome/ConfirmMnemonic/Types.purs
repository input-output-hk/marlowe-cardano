module Page.Welcome.ConfirmMnemonic.Types where

import Capability.Marlowe (NewWalletDetails)
import Halogen as H
import Type.Proxy (Proxy(..))

type Input =
  { newWalletDetails :: NewWalletDetails
  }

data Msg
  = BackClicked NewWalletDetails
  | MnemonicConfirmed NewWalletDetails

data Query (a :: Type)

type Slot m = H.Slot Query Msg m

type Component m = H.Component Query Input Msg m

_mnemonic = Proxy :: Proxy "mnemonic"
