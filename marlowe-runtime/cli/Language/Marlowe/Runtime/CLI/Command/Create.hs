module Language.Marlowe.Runtime.CLI.Command.Create
  where

import Data.Map (Map)
import Language.Marlowe (POSIXTime)
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.ChainSync.Api (Address, TokenName)
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion)
import Options.Applicative

data CreateCommand = CreateCommand
  { marloweVersion :: SomeMarloweVersion
  , roles :: Map TokenName Address
  , contractFiles :: ContractFiles
  }

data ContractFiles
  = CoreFile FilePath
  | ExtendedFiles FilePath ContractArgs

data ContractArgs
  = ContractArgsByFile FilePath
  | ContractArgsByValue ContractArgsValue

data ContractArgsValue = ContractArgsValue
  { timeoutArguments :: Map String POSIXTime
  , valueArguments :: Map String Integer
  }

createCommandParser :: ParserInfo (TxCommand CreateCommand)
createCommandParser = info empty $ progDesc "Create a new Marlowe Contract"

runCreateCommand :: TxCommand CreateCommand -> CLI ()
runCreateCommand = error "not implemented"
