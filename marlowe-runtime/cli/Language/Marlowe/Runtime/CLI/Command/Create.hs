module Language.Marlowe.Runtime.CLI.Command.Create
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data CreateCommand

createCommandParser :: ParserInfo CreateCommand
createCommandParser = error "not implemented"

runCreateCommand :: CreateCommand -> CLI ()
runCreateCommand = error "not implemented"
