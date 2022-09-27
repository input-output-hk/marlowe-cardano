module Language.Marlowe.Runtime.CLI.Command.Create
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data CreateCommand

createCommandParser :: ParserInfo CreateCommand
createCommandParser = info empty $ progDesc "Create a new Marlowe Contract"

runCreateCommand :: CreateCommand -> CLI ()
runCreateCommand = error "not implemented"
