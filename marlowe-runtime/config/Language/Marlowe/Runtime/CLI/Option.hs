module Language.Marlowe.Runtime.CLI.Option
  where

import Control.Arrow ((>>>))
import Language.Marlowe.Runtime.Core.Api (ContractId, parseContractId)
import Network.Socket (HostName, PortNumber)
import Options.Applicative
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- | Describes how to extract an option from an environment variable and from a
-- command line option.
data CliOption f a = CliOption
  { env :: !String
  , parseEnv :: !(String -> Maybe a)
  , parser :: !(Mod f a -> Parser a)
  }

historyHost :: CliOption OptionFields HostName
historyHost = host "history" "HISTORY" "127.0.0.1" "The hostname of the Marlowe Runtime history server."

historyCommandPort :: CliOption OptionFields PortNumber
historyCommandPort = port "history-command" "HISTORY_COMMAND" 3717 "The port number of the history server's job API."

historyQueryPort :: CliOption OptionFields PortNumber
historyQueryPort = port "history-query" "HISTORY_QUERY" 3718 "The port number of the history server's query API."

historySyncPort :: CliOption OptionFields PortNumber
historySyncPort = port "history-sync" "HISTORY_SYNC" 3719 "The port number of the history server's synchronization API."

txHost :: CliOption OptionFields HostName
txHost = host "tx" "TX" "127.0.0.1" "The hostname of the Marlowe Runtime transaction server."

txCommandPort :: CliOption OptionFields PortNumber
txCommandPort = port "tx-command" "TX_COMMAND" 3720 "The port number of the transaction server's job API."

port :: String -> String -> PortNumber -> String -> CliOption OptionFields PortNumber
port optPrefix envPrefix defaultValue description = CliOption
  { env = "MARLOWE_RT_" <> envPrefix <> "_PORT"
  , parseEnv = readMaybe
  , parser = option auto . (<>) (mconcat
      [ long $ optPrefix <> "-port"
      , value defaultValue
      , metavar "PORT_NUMBER"
      , help description
      , showDefault
      ])
  }

host :: String -> String -> HostName  -> String -> CliOption OptionFields HostName
host optPrefix envPrefix defaultValue description = CliOption
  { env = "MARLOWE_RT_" <> envPrefix <> "_HOST"
  , parseEnv = Just
  , parser = strOption . (<>) (mconcat
      [ long $ optPrefix <> "-host"
      , value defaultValue
      , metavar "HOST_NAME"
      , help description
      , showDefault
      ])
  }

contractIdArgument :: String -> Parser ContractId
contractIdArgument description = argument parser $ mconcat
  [ metavar "CONTRACT_ID"
  , help description
  ]
  where
    parser = eitherReader $ parseContractId >>> \case
      Nothing  -> Left "Invalid contract ID - expected format: <hex-tx-id>#<tx-out-ix>"
      Just cid -> Right cid

optParserWithEnvDefault :: HasValue f => CliOption f a -> IO (Parser a)
optParserWithEnvDefault CliOption{..} = do
  envMod <- foldMap value . (parseEnv =<<) <$> lookupEnv env
  pure $ parser envMod
