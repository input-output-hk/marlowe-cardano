module Language.Marlowe.Runtime.CLI.Command.Rm
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data RmCommand

rmCommandParser :: ParserInfo RmCommand
rmCommandParser = error "not implemented"

runRmCommand :: RmCommand -> CLI ()
runRmCommand = error "not implemented"
