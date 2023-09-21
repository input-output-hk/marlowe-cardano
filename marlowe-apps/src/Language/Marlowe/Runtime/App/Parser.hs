module Language.Marlowe.Runtime.App.Parser (
  addressKeyfileParser,
  addressParser,
  getConfigParser,
) where

import Data.Default (def)
import Language.Marlowe.Runtime.App.Types (
  Config (Config, buildSeconds, confirmSeconds, retryLimit, retrySeconds, timeoutSeconds),
 )
import Language.Marlowe.Runtime.CLI.Option (optParserWithEnvDefault)
import Language.Marlowe.Runtime.ChainSync.Api (Address, fromBech32)

import qualified Data.Text as T (pack)
import qualified Language.Marlowe.Runtime.CLI.Option as CLI
import qualified Options.Applicative as O

getConfigParser :: IO (O.Parser Config)
getConfigParser =
  do
    runtimeHostParser <- optParserWithEnvDefault CLI.runtimeHost
    runtimePortParser <- optParserWithEnvDefault CLI.runtimePort
    let timeoutSecondsParser =
          O.option O.auto $
            O.long "timeout-seconds"
              <> O.metavar "INTEGER"
              <> O.value (timeoutSeconds def)
              <> O.showDefault
              <> O.help "Timeout in seconds for transaction confirmation."
        buildSecondsParser =
          O.option O.auto $
            O.long "build-seconds"
              <> O.metavar "INTEGER"
              <> O.value (buildSeconds def)
              <> O.showDefault
              <> O.help
                "Wait specified seconds before transaction construction. No waiting occurs if a non-positive number of seconds is specified. The specified wait period is randomly increased up to a factor of two. Increasing this value will increase the probability that Marlowe Runtime's node has seen the transactions that the submitting node has seen."
        confirmSecondsParser =
          O.option O.auto $
            O.long "confirm-seconds"
              <> O.metavar "INTEGER"
              <> O.value (confirmSeconds def)
              <> O.showDefault
              <> O.help
                "Wait specified seconds after transaction confirmation. No waiting occurs if a non-positive number of seconds is specified. The specified wait period is randomly increased up to a factor of two. Increasing this value will increase the probability that the submitting node has seen the transactions that Marlowe Runtime has seen."
        retrySecondsParser =
          O.option O.auto $
            O.long "retry-seconds"
              <> O.metavar "INTEGER"
              <> O.value (retrySeconds def)
              <> O.showDefault
              <> O.help
                "Wait specified seconds after after a failed transaction before trying again. No retries occur if a non-positive number of seconds is specified."
        retryLimitParser =
          O.option O.auto $
            O.long "retry-limit"
              <> O.metavar "INTEGER"
              <> O.value (retryLimit def)
              <> O.showDefault
              <> O.help
                "Maximum number of attempts for trying a failed transaction again. Each subsequent retry waits twice as long as the previous retry. No retries occur if a non-positive number of retries is specified."
    pure $
      Config
        <$> runtimeHostParser
        <*> runtimePortParser
        <*> timeoutSecondsParser
        <*> buildSecondsParser
        <*> confirmSecondsParser
        <*> retrySecondsParser
        <*> retryLimitParser

addressParser :: O.ReadM Address
addressParser = O.maybeReader $ fromBech32 . T.pack

addressKeyfileParser :: O.ReadM (Address, FilePath)
addressKeyfileParser =
  O.eitherReader $
    \s ->
      case break (== '=') s of
        (_, []) -> Left "Missing key filename."
        (address, _ : key) -> case fromBech32 $ T.pack address of
          Just address' -> Right (address', key)
          Nothing -> Left "Filed to parse Bech32 address."
