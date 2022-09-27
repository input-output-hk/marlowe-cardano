module Language.Marlowe.Runtime.CLI.Command.Apply
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data ApplyCommand

applyCommandParser :: ParserInfo ApplyCommand
applyCommandParser = info empty $ progDesc "Apply inputs to a contract"

depositCommandParser :: ParserInfo ApplyCommand
depositCommandParser = info empty $ progDesc "Deposit funds into a contract"

chooseCommandParser :: ParserInfo ApplyCommand
chooseCommandParser = info empty $ progDesc "Make a choice in a contract"

notifyCommandParser :: ParserInfo ApplyCommand
notifyCommandParser = info empty $ progDesc "Notify a contract to proceed"

runApplyCommand :: ApplyCommand -> CLI ()
runApplyCommand = error "not implemented"
