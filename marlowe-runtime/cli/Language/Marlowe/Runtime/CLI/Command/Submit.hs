module Language.Marlowe.Runtime.CLI.Command.Submit
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data SubmitCommand

submitCommandParser :: ParserInfo SubmitCommand
submitCommandParser = error "not implemented"

runSubmitCommand :: SubmitCommand -> CLI ()
runSubmitCommand = error "not implemented"
