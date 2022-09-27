module Language.Marlowe.Runtime.CLI.Command.Withdraw
  where

import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (TokenName)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Options.Applicative

data WithdrawCommand = WithdrawCommand
  { contractId :: ContractId
  , role :: TokenName
  }

withdrawCommandParser :: ParserInfo (TxCommand WithdrawCommand)
withdrawCommandParser = info empty $ progDesc "Withdraw funds paid to a role in a contract"

runWithdrawCommand :: TxCommand WithdrawCommand -> CLI ()
runWithdrawCommand = error "not implemented"
