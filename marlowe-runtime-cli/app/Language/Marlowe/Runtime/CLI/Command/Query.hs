module Language.Marlowe.Runtime.CLI.Command.Query where

import Language.Marlowe.Runtime.CLI.Command.Query.Store
import Language.Marlowe.Runtime.CLI.Monad
import Options.Applicative

newtype QueryCommand
  = StoreQuery StoreQueryCommand

queryCommandParser :: ParserInfo QueryCommand
queryCommandParser = info parser $ progDesc "Query the runtime"
  where
    parser =
      hsubparser $
        mconcat
          [ command "store" $ StoreQuery <$> storeQueryCommandParser
          ]

runQueryCommand :: QueryCommand -> CLI ()
runQueryCommand = \case
  StoreQuery cmd -> runStoreQueryCommand cmd
