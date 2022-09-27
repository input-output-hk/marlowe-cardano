module Language.Marlowe.Runtime.CLI.Command.Withdraw
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data WithdrawCommand

withdrawCommandParser :: ParserInfo WithdrawCommand
withdrawCommandParser = info empty $ progDesc "Withdraw funds paid to a role in a contract"

runWithdrawCommand :: WithdrawCommand -> CLI ()
runWithdrawCommand = error "not implemented"
