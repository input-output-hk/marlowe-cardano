module Language.Marlowe.Runtime.CLI.Command.Withdraw
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data WithdrawCommand

withdrawCommandParser :: ParserInfo WithdrawCommand
withdrawCommandParser = error "not implemented"

runWithdrawCommand :: WithdrawCommand -> CLI ()
runWithdrawCommand = error "not implemented"
