module Language.Marlowe.Runtime.CLI.Command.Create
  where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (fromString)
import Language.Marlowe (POSIXTime(POSIXTime))
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand, txCommandParser)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.CLI.Option (keyValueOption, marloweVersionParser, parseAddress)
import Language.Marlowe.Runtime.ChainSync.Api (Address, PolicyId, TokenName(..))
import Language.Marlowe.Runtime.Core.Api (SomeMarloweVersion)
import Options.Applicative
import Text.Read (readMaybe)

data CreateCommand = CreateCommand
  { marloweVersion :: SomeMarloweVersion
  , roles :: RolesConfig
  , contractFiles :: ContractFiles
  }

data RolesConfig
  = MintSimple (Map TokenName Address)
  | MintConfig FilePath
  | UseExistingPolicyId PolicyId

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
    rolesParser = mintSimpleParser <|> mintConfigParser <|> policyIdParser
    mintSimpleParser = MintSimple . Map.fromList <$> many roleParser
    mintConfigParser = fmap MintConfig $ strOption $ mconcat
      [ long "roles-config-file"
      , help $ unwords
          [ "A JSON file containing a map of role token names to a roles configuration object."
          , "The roles configuration object has two keys, \"address\" and \"metadata\","
          , "where \"address\" is the address to send the newly minted role token and \"metadata\""
          , "is the CIP-25 metadata object to associate with the token."
          ]
      , metavar "FILE_PATH"
      ]
    policyIdParser = fmap UseExistingPolicyId $ strOption $ mconcat
      [ long "role-token-policy-id"
      , metavar "POLICY_ID"
      , help "The hexadecimal-encoded policy ID of the role tokens for this contract. This option is used to support role tokens minted in a separate transaction."
      ]
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
      , help "The name of a timeout parameter in the contract and a value to assign to it (in POSIX milliseconds)."
      ]
    valueArgumentParser = keyValueOption Right integerParser $ mconcat
      [ long "value-arg"
      , metavar "NAME=INTEGER"
      , help "The name of a numeric parameter in the contract and a value to assign to it."
      ]
    integerParser = maybe (Left "Invalid Integer value") Right . readMaybe

runCreateCommand :: TxCommand CreateCommand -> CLI ()
runCreateCommand = error "not implemented"
