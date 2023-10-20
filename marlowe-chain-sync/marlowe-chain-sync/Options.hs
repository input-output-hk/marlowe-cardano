module Options (
  Options (..),
  getOptions,
) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Data.Maybe (fromMaybe)
import Network.Socket (HostName, PortNumber)
import qualified Options.Applicative as O
import Prettyprinter
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket :: !FilePath
  , networkId :: !NetworkId
  , databaseUri :: !String
  , host :: !HostName
  , port :: !PortNumber
  , queryPort :: !PortNumber
  , commandPort :: !PortNumber
  , httpPort :: !PortNumber
  }
  deriving (Show, Eq)

getOptions :: String -> IO Options
getOptions version = do
  defaultNetworkId <- O.value . fromMaybe Mainnet <$> readNetworkId
  defaultSocketPath <- maybe mempty O.value <$> readSocketPath
  defaultDatabaseUri <- maybe mempty O.value <$> readDatabaseUri
  defaultHost <- O.value . fromMaybe "127.0.0.1" <$> readHost
  defaultPort <- O.value . fromMaybe 3715 <$> readPort
  defaultQueryPort <- O.value . fromMaybe 3716 <$> readQueryPort
  defaultJobPort <- O.value . fromMaybe 3720 <$> readJobPort
  O.execParser $
    parseOptions
      defaultNetworkId
      defaultSocketPath
      defaultDatabaseUri
      defaultHost
      defaultPort
      defaultQueryPort
      defaultJobPort
      version
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
        _ -> value

    readDatabaseUri :: IO (Maybe String)
    readDatabaseUri = do
      value <- lookupEnv "CHAIN_SYNC_DB_URI"
      pure case value of
        Just "" -> Nothing
        _ -> value

    readHost :: IO (Maybe HostName)
    readHost = do
      value <- lookupEnv "CHAIN_SYNC_HOST"
      pure case value of
        Just "" -> Nothing
        _ -> value

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
    parser =
      O.helper
        <*> versionOption
        <*> ( Options
                <$> socketPathOption
                <*> networkIdOption
                <*> databaseUriOption
                <*> hostOption
                <*> portOption
                <*> queryPortOption
                <*> jobPortOption
                <*> httpPortOption
            )
      where
        versionOption :: O.Parser (a -> a)
        versionOption =
          O.infoOption
            ("marlowe-chain-sync " <> version)
            (O.long "version" <> O.help "Show version.")

        socketPathOption :: O.Parser FilePath
        socketPathOption = O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options =
              mconcat
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
            options =
              mconcat
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
            options =
              mconcat
                [ O.long "testnet-magic"
                , O.short 'm'
                , O.metavar "INTEGER"
                , defaultNetworkId
                , O.help "Testnet network ID magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable."
                ]

        portOption :: O.Parser PortNumber
        portOption =
          O.option O.auto $
            mconcat
              [ O.long "port"
              , defaultPort
              , O.metavar "PORT_NUMBER"
              , O.help "The port number to serve the chain sync protocol on."
              , O.showDefault
              ]

        queryPortOption :: O.Parser PortNumber
        queryPortOption =
          O.option O.auto $
            mconcat
              [ O.long "query-port"
              , defaultQueryPort
              , O.metavar "PORT_NUMBER"
              , O.help "The port number to serve the query protocol on."
              , O.showDefault
              ]

        jobPortOption :: O.Parser PortNumber
        jobPortOption =
          O.option O.auto $
            mconcat
              [ O.long "job-port"
              , defaultJobPort
              , O.metavar "PORT_NUMBER"
              , O.help "The port number to serve the job protocol on."
              , O.showDefault
              ]

        hostOption :: O.Parser HostName
        hostOption =
          O.strOption $
            mconcat
              [ O.long "host"
              , O.short 'h'
              , defaultHost
              , O.metavar "HOST_NAME"
              , O.help "The hostname to serve the chain sync protocol on."
              , O.showDefault
              ]

        httpPortOption :: O.Parser PortNumber
        httpPortOption =
          O.option O.auto $
            mconcat
              [ O.long "http-port"
              , defaultPort
              , O.metavar "PORT_NUMBER"
              , O.help "Port number to serve the http healthcheck API on"
              , O.value 8080
              , O.showDefault
              ]

    infoMod :: O.InfoMod Options
    infoMod =
      mconcat
        [ O.fullDesc
        , O.progDescDoc $ Just description
        , O.header "marlowe-chain-indexer: Chain query and sync server for the Marlowe Runtime."
        ]

description :: Doc ann
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "The chain query engine for the Marlowe Runtime. This component exposes three"
        , "protocols through which downstream components can interact with the blockchain."
        , "These are: chain seek, chain query, and chain command."
        ]
    , vcat
        [ "The chain seek protocol is a synchronization protocol which allows the follower"
        , "to jump directly ahead to blocks that match a particular query."
        ]
    , vcat
        [ "The chain query protocol allows various network parameters and UTxO state to be"
        , "queried."
        ]
    , vcat
        [ "The chain command protocol allows transactions to be submitted to the connected node."
        ]
    , vcat
        [ "marlowe-chain-sync relies on the connected database being migrated and populated by"
        , "a marlowe-chain-indexer instance. While marlowe-chain-sync can operate without"
        , "marlowe-chain-indexer running, marlowe-chain-indexer must be run first to insert"
        , "the genesis UTxOs before marlowe-chain-sync can be used, and relies on"
        , "marlowe-chain-indexer to keep the database up-to-date."
        ]
    , vcat
        [ "marlowe-chain-sync is designed to scale horizontally. That is to say, multiple"
        , "instances can run in parallel to scale with demand. A typical setup for this would"
        , "involve running multiple marlowe-chain-sync instances in front of a load balancer"
        , "against a scalable postgres replica cluster being populated by a single chain indexer."
        ]
    ]
