module Language.Marlowe.Runtime.CLI.Command.Tx
  where

import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses)

data TxCommand cmd = TxCommand
  { walletAddresses :: WalletAddresses
  , signingMethod :: SigningMethod
  , subCommand :: cmd
  }

data SigningMethod
  = Manual
