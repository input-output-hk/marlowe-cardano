module Language.Marlowe.Runtime.CLI.Command.Submit
  where

import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Options.Applicative

newtype SubmitCommand = SubmitCommand
  { txFile :: FilePath
  }

submitCommandParser :: ParserInfo SubmitCommand
submitCommandParser = info parser $ progDesc "Submit a signed transaction to the Cardano node. Expects the CBOR bytes of the signed Tx from stdin."
  where
    parser = SubmitCommand
      <$> txFileParser
    txFileParser = strArgument $ mconcat
      [ metavar "FILE_PATH"
      , help "A file containing the CBOR bytes of the signed transaction to submit."
      ]

runSubmitCommand :: SubmitCommand -> CLI ()
runSubmitCommand = error "not implemented"
