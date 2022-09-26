module Language.Marlowe.Runtime.CLI.Command.Add
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data AddCommand

addCommandParser :: ParserInfo AddCommand
addCommandParser = error "not implemented"

runAddCommand :: AddCommand -> CLI ()
runAddCommand = error "not implemented"
