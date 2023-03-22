module Options
  ( Options(..)
  , getOptions
  ) where

import Cardano.Api (NetworkId(..), NetworkMagic(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Logging (defaultRootSelectorLogConfig)
import Network.Socket (HostName, PortNumber)
import qualified Options.Applicative as O
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket        :: !FilePath
  , networkId         :: !NetworkId
  , databaseUri       :: !String
  , host              :: !HostName
  , port              :: !PortNumber
  , queryPort         :: !PortNumber
  , commandPort       :: !PortNumber
  , logConfigFile     :: !(Maybe FilePath)
  , httpPort :: !PortNumber
  } deriving (Show, Eq)

getOptions :: String -> IO Options
getOptions version = do
  defaultNetworkId <- O.value . fromMaybe Mainnet <$> readNetworkId
  defaultSocketPath <- maybe mempty O.value <$> readSocketPath
  defaultDatabaseUri <- maybe mempty O.value <$> readDatabaseUri
  defaultHost <- O.value . fromMaybe "127.0.0.1" <$> readHost
  defaultPort <- O.value . fromMaybe 3715 <$> readPort
  defaultQueryPort <- O.value . fromMaybe 3716 <$> readQueryPort
  defaultJobPort <- O.value . fromMaybe 3720 <$> readJobPort
  O.execParser $ parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri defaultHost defaultPort defaultQueryPort defaultJobPort version
  where
    readNetworkId :: IO (Maybe NetworkId)
    readNetworkId = do
      value <- lookupEnv "CARDANO_TESTNET_MAGIC"
      pure $ Testnet . NetworkMagic <$> (readMaybe =<< value)

    readSocketPath :: IO (Maybe FilePath)
    readSocketPath = do
      value <- lookupEnv "CARDANO_NODE_SOCKET_PATH"
      pure case value of
        Just "" -> Nothing
        _       -> value

    readDatabaseUri :: IO (Maybe String)
    readDatabaseUri = do
      value <- lookupEnv "CHAIN_SYNC_DB_URI"
      pure case value of
        Just "" -> Nothing
        _       -> value

    readHost :: IO (Maybe HostName)
    readHost = do
      value <- lookupEnv "CHAIN_SYNC_HOST"
      pure case value of
        Just "" -> Nothing
        _       -> value

    readPort :: IO (Maybe PortNumber)
    readPort = do
      value <- lookupEnv "CHAIN_SYNC_PORT"
      pure $ readMaybe =<< value

    readQueryPort :: IO (Maybe PortNumber)
    readQueryPort = do
      value <- lookupEnv "CHAIN_SYNC_QUERY_PORT"
      pure $ readMaybe =<< value

    readJobPort :: IO (Maybe PortNumber)
    readJobPort = do
      value <- lookupEnv "CHAIN_SYNC_JOB_PORT"
      pure $ readMaybe =<< value

parseOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.OptionFields String
  -> O.Mod O.OptionFields HostName
  -> O.Mod O.OptionFields PortNumber
  -> O.Mod O.OptionFields PortNumber
  -> O.Mod O.OptionFields PortNumber
  -> String
  -> O.ParserInfo Options
parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri defaultHost defaultPort defaultQueryPort defaultJobPort version = O.info parser infoMod
  where
    parser :: O.Parser Options
    parser = O.helper
      <*> versionOption
      <*> printLogConfigOption
      <*> ( Options
              <$> socketPathOption
              <*> networkIdOption
              <*> databaseUriOption
              <*> hostOption
              <*> portOption
              <*> queryPortOption
              <*> jobPortOption
              <*> logConfigFileParser
              <*> httpPortOption
          )
      where
        versionOption :: O.Parser (a -> a)
        versionOption = O.infoOption
          ("marlowe-chain-sync " <> version)
          (O.long "version" <> O.help "Show version.")

        printLogConfigOption :: O.Parser (a -> a)
        printLogConfigOption = O.infoOption
          (T.unpack $ decodeUtf8 $ encodePretty defaultRootSelectorLogConfig)
          (O.long "print-log-config" <> O.help "Print the default log configuration.")

        socketPathOption :: O.Parser FilePath
        socketPathOption = O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options = mconcat
              [ O.long "socket-path"
              , O.short 's'
              , O.metavar "SOCKET_FILE"
              , defaultSocketPath
              , O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable."
              ]

        databaseUriOption :: O.Parser String
        databaseUriOption = O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options = mconcat
              [ O.long "database-uri"
              , O.short 'd'
              , O.metavar "DATABASE_URI"
              , defaultDatabaseUri
              , O.help "URI of the database where the chain information is saved."
              ]

        networkIdOption :: O.Parser NetworkId
        networkIdOption = O.option parse options
          where
            parse :: O.ReadM NetworkId
            parse = Testnet . NetworkMagic . toEnum <$> O.auto

            options :: O.Mod O.OptionFields NetworkId
            options = mconcat
              [ O.long "testnet-magic"
              , O.short 'm'
              , O.metavar "INTEGER"
              , defaultNetworkId
              , O.help "Testnet network ID magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable."
              ]

        portOption :: O.Parser PortNumber
        portOption = O.option O.auto $ mconcat
          [ O.long "port"
          , defaultPort
          , O.metavar "PORT_NUMBER"
          , O.help "The port number to serve the chain sync protocol on."
          , O.showDefault
          ]

        queryPortOption :: O.Parser PortNumber
        queryPortOption = O.option O.auto $ mconcat
          [ O.long "query-port"
          , defaultQueryPort
          , O.metavar "PORT_NUMBER"
          , O.help "The port number to serve the query protocol on."
          , O.showDefault
          ]

        jobPortOption :: O.Parser PortNumber
        jobPortOption = O.option O.auto $ mconcat
          [ O.long "job-port"
          , defaultJobPort
          , O.metavar "PORT_NUMBER"
          , O.help "The port number to serve the job protocol on."
          , O.showDefault
          ]

        hostOption :: O.Parser HostName
        hostOption = O.strOption $ mconcat
          [ O.long "host"
          , O.short 'h'
          , defaultHost
          , O.metavar "HOST_NAME"
          , O.help "The hostname to serve the chain sync protocol on."
          , O.showDefault
          ]

        logConfigFileParser :: O.Parser (Maybe FilePath)
        logConfigFileParser = O.optional $ O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options = mconcat
              [ O.long "log-config-file"
              , O.metavar "FILE_PATH"
              , O.help "Path to the log configuration JSON file."
              ]

        httpPortOption :: O.Parser PortNumber
        httpPortOption = O.option O.auto $ mconcat
          [ O.long "http-port"
          , defaultPort
          , O.metavar "PORT_NUMBER"
          , O.help "Port number to serve the http healthcheck API on"
          , O.value 8080
          , O.showDefault
          ]

    infoMod :: O.InfoMod Options
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Chain sync server for Marlowe Runtime."
      , O.header "marlowe-chain-sync : a chain sync server for the Marlowe Runtime."
      ]
