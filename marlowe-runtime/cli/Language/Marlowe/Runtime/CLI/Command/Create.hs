module Language.Marlowe.Runtime.CLI.Command.Create
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Language.Marlowe (POSIXTime(POSIXTime))
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand, txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.CLI.Option (keyValueOption, marloweVersionParser, parseAddress)
import Language.Marlowe.Runtime.ChainSync.Api (Address, TokenName(..))
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion)
import Options.Applicative
import Text.Read (readMaybe)

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
createCommandParser = info (txCommandParser parser) $ progDesc "Create a new Marlowe Contract"
  where
    parser = CreateCommand
      <$> marloweVersionParser
      <*> rolesParser
      <*> contractFilesParser
    rolesParser = Map.fromList <$> many roleParser
    roleParser = keyValueOption (Right . TokenName . fromString) parseAddress $ mconcat
      [ long "role"
      , short 'r'
      , help "The name of a role in the contract with the address to send the token to"
      , metavar "ROLE=ADDRESS"
      ]
    contractFilesParser = CoreFile <$> coreParser <|> extendedParser
    coreParser = strOption $ mconcat
      [ long "core-file"
      , help "A file containing the Core Marlowe JSON definition of the contract to create."
      , metavar "FILE_PATH"
      ]
    extendedParser = ExtendedFiles <$> extendedFileParser <*> contractArgsParser
    extendedFileParser = strOption $ mconcat
      [ long "contract-file"
      , help "A file containing the Extended Marlowe JSON definition of the contract to create."
      , metavar "FILE_PATH"
      ]
    contractArgsParser =
      ContractArgsByFile <$> argsFileParser <|> ContractArgsByValue <$> contractArgsValueParser
    argsFileParser = strOption $ mconcat
      [ long "args-file"
      , help "A file containing the Extended Marlowe arguments to apply to the contract."
      , metavar "FILE_PATH"
      ]
    contractArgsValueParser = ContractArgsValue
      <$> timeoutArgumentsParser
      <*> valueArgumentsParser
    timeoutArgumentsParser = Map.fromList <$> many timeoutArgumentParser
    valueArgumentsParser = Map.fromList <$> many valueArgumentParser
    timeoutArgumentParser = keyValueOption Right (fmap POSIXTime . integerParser) $ mconcat
      [ long "timeout-arg"
      , metavar "NAME=POSIX_TIMESTAMP"
      ]
    valueArgumentParser = keyValueOption Right integerParser $ mconcat
      [ long "value-arg"
      , metavar "NAME=INT"
      ]
    integerParser = maybe (Left "Invalid Integer value") Right . readMaybe

runCreateCommand :: TxCommand CreateCommand -> CLI ()
runCreateCommand = error "not implemented"
