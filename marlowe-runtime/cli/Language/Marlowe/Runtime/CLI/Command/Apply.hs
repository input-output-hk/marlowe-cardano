module Language.Marlowe.Runtime.CLI.Command.Apply
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data ApplyCommand

applyCommandParser :: ParserInfo ApplyCommand
applyCommandParser = error "not implemented"

depositCommandParser :: ParserInfo ApplyCommand
depositCommandParser = error "not implemented"

chooseCommandParser :: ParserInfo ApplyCommand
chooseCommandParser = error "not implemented"

notifyCommandParser :: ParserInfo ApplyCommand
notifyCommandParser = error "not implemented"

runApplyCommand :: ApplyCommand -> CLI ()
runApplyCommand = error "not implemented"
