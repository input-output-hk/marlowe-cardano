module Language.Marlowe.Runtime.CLI.Command.Submit
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

data SubmitCommand = SubmitCommand

submitCommandParser :: ParserInfo SubmitCommand
submitCommandParser = info (pure SubmitCommand) $ progDesc "Submit a signed transaction to the Cardano node."

runSubmitCommand :: SubmitCommand -> CLI ()
runSubmitCommand = error "not implemented"
