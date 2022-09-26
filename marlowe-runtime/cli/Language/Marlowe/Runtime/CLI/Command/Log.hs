module Language.Marlowe.Runtime.CLI.Command.Log
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data LogCommand

logCommandParser :: ParserInfo LogCommand
logCommandParser = error "not implemented"

runLogCommand :: LogCommand -> CLI ()
runLogCommand = error "not implemented"
