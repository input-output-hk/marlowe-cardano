

module Language.Marlowe.Runtime.App.Parser
  ( getConfigParser
  ) where


import Language.Marlowe.Runtime.App.Types (Config(Config))
import Language.Marlowe.Runtime.CLI.Option (CliOption, host, optParserWithEnvDefault, port)
import Network.Socket (HostName, PortNumber)

import qualified Language.Marlowe.Runtime.CLI.Option as CLI
import qualified Options.Applicative as O


getConfigParser :: IO (O.Parser Config)
getConfigParser =
  do
    chainSeekHostParser <- optParserWithEnvDefault chainSeekHost
    chainSeekCommandPortParser <- optParserWithEnvDefault chainSeekCommandPort
    chainSeekQueryPortParser <- optParserWithEnvDefault chainSeekQueryPort
    chainSeekSyncPortParser <- optParserWithEnvDefault chainSeekSyncPort
    historyHostParser <- optParserWithEnvDefault CLI.historyHost
    historyCommandPortParser <- optParserWithEnvDefault CLI.historyCommandPort
    historyQueryPortParser <- optParserWithEnvDefault CLI.historyQueryPort
    historySyncPortParser <- optParserWithEnvDefault CLI.historySyncPort
    discoveryHostParser <- optParserWithEnvDefault CLI.discoveryHost
    discoveryQueryPortParser <- optParserWithEnvDefault CLI.discoveryQueryPort
    discoverySyncPortParser <- optParserWithEnvDefault CLI.discoverySyncPort
    txHostParser <- optParserWithEnvDefault CLI.txHost
    txCommandPortParser <- optParserWithEnvDefault CLI.txCommandPort
    let
      timeoutSecondsParser =
        O.option O.auto
          $  O.long "timeout-seconds"
          <> O.metavar "INTEGER"
          <> O.value 600
          <> O.help "Time timeout in seconds for transaction confirmation."
    pure
      $ Config
      <$> chainSeekHostParser
      <*> chainSeekCommandPortParser
      <*> chainSeekQueryPortParser
      <*> chainSeekSyncPortParser
      <*> historyHostParser
      <*> historyCommandPortParser
      <*> historyQueryPortParser
      <*> historySyncPortParser
      <*> discoveryHostParser
      <*> discoveryQueryPortParser
      <*> discoverySyncPortParser
      <*> txHostParser
      <*> txCommandPortParser
      <*> timeoutSecondsParser


chainSeekHost :: CliOption O.OptionFields HostName
chainSeekHost = host "chain-seek" "CHAINSEEK" "127.0.0.1" "The hostname of the Marlowe Runtime chain-seek server."


chainSeekCommandPort :: CliOption O.OptionFields PortNumber
chainSeekCommandPort = port "chain-seek-command" "CHAINSEEK_COMMAND" 3720 "The port number of the chain-seek server's job API."


chainSeekQueryPort :: CliOption O.OptionFields PortNumber
chainSeekQueryPort = port "chain-seek-query" "CHAINSEEK_QUERY" 3716 "The port number of the chain-seek server's query API."


chainSeekSyncPort :: CliOption O.OptionFields PortNumber
chainSeekSyncPort = port "chain-seek-sync" "CHAINSEEK_SYNC" 3715 "The port number of the chain-seek server's synchronization API."
