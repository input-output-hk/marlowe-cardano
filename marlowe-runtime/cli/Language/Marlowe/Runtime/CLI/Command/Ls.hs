module Language.Marlowe.Runtime.CLI.Command.Ls
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data LsCommand

lsCommandParser :: ParserInfo LsCommand
lsCommandParser = error "not implemented"

runLsCommand :: LsCommand -> CLI ()
runLsCommand = error "not implemented"
