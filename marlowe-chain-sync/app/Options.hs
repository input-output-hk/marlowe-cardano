module Options
  ( Options(..)
  , getOptions
  ) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Language.Marlowe.Runtime.ChainSync.NodeClient (CostModel (..))
import Network.Socket (HostName, PortNumber)
import qualified Options.Applicative as O
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket        :: !FilePath
  , networkId         :: !NetworkId
  , databaseUri       :: !String
  , genesisConfigHash :: !Text
  , genesisConfigFile :: !FilePath
  , host              :: !HostName
  , port              :: !PortNumber
  , queryPort         :: !PortNumber
  , costModel         :: !CostModel
  , maxCost           :: !Int
  } deriving (Show, Eq)

getOptions :: String -> IO Options
getOptions version = do
  defaultNetworkId <- O.value . fromMaybe Mainnet <$> readNetworkId
  defaultSocketPath <- maybe mempty O.value <$> readSocketPath
  defaultDatabaseUri <- maybe mempty O.value <$> readDatabaseUri
  defaultHost <- O.value . fromMaybe "127.0.0.1" <$> readHost
  defaultPort <- O.value . fromMaybe 3715 <$> readPort
  defaultQueryPort <- O.value . fromMaybe 3716 <$> readQueryPort
  O.execParser $ parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri defaultHost defaultPort defaultQueryPort version
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

parseOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.OptionFields String
  -> O.Mod O.OptionFields HostName
  -> O.Mod O.OptionFields PortNumber
  -> O.Mod O.OptionFields PortNumber
  -> String
  -> O.ParserInfo Options
parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri defaultHost defaultPort defaultQueryPort version = O.info parser infoMod
  where
    parser :: O.Parser Options
    parser = O.helper
      <*> versionOption
      <*> ( Options
              <$> socketPathOption
              <*> networkIdOption
              <*> databaseUriOption
              <*> genesisConfigHashOption
              <*> genesisConfigFileOption
              <*> hostOption
              <*> portOption
              <*> queryPortOption
              <*> costModelParser
              <*> maxCostParser
          )
      where
        versionOption :: O.Parser (a -> a)
        versionOption = O.infoOption
          ("chainseekd " <> version)
          (O.long "version" <> O.help "Show version.")

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

        genesisConfigFileOption :: O.Parser String
        genesisConfigFileOption = O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options = mconcat
              [ O.long "genesis-config-file"
              , O.metavar "CONFIG_FILE"
              , O.help "Path to the Byron Genesis Config JSON File."
              ]

        genesisConfigHashOption :: O.Parser Text
        genesisConfigHashOption = O.strOption options
          where
            options :: O.Mod O.OptionFields Text
            options = mconcat
              [ O.long "genesis-config-file-hash"
              , O.metavar "CONFIG_HASH"
              , O.help "Hash of the Byron Genesis Config JSON file."
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
          [ O.long "port-number"
          , defaultPort
          , O.metavar "PORT_NUMBER"
          , O.help "The port number to serve the chain seek protocol on. Default value: 3715"
          ]

        queryPortOption :: O.Parser PortNumber
        queryPortOption = O.option O.auto $ mconcat
          [ O.long "query-port-number"
          , defaultQueryPort
          , O.metavar "PORT_NUMBER"
          , O.help "The port number to serve the query protocol on. Default value: 3716"
          ]

        hostOption :: O.Parser HostName
        hostOption = O.strOption $ mconcat
          [ O.long "host"
          , O.short 'h'
          , defaultHost
          , O.metavar "HOST_NAME"
          , O.help "The hostname to serve the chain seek protocol on. Default value: 127.0.0.1"
          ]

        costModelParser :: O.Parser CostModel
        costModelParser = CostModel <$> blockCostParser <*> txCostParser

        blockCostParser :: O.Parser Int
        blockCostParser = O.option O.auto $ mconcat
          [ O.long "block-cost"
          , O.value 1
          , O.metavar "COST_UNITS"
          , O.help "The number of cost units to associate with persisting a block when computing the cost model. Default value: 1"
          ]

        txCostParser :: O.Parser Int
        txCostParser = O.option O.auto $ mconcat
          [ O.long "tx-cost"
          , O.value 10
          , O.metavar "COST_UNITS"
          , O.help "The number of cost units to associate with persisting a transaction when computing the cost model. Default value: 10"
          ]

        maxCostParser :: O.Parser Int
        maxCostParser = O.option O.auto $ mconcat
          [ O.long "max-cost"
          , O.value 100_000
          , O.metavar "COST_UNITS"
          , O.help "The maximum number of cost units that can be batched when persisting blocks. If the cost of the current batch would exceed this value, the chain sync client will wait until the current batch is persisted before requesting another block. Default value: 100,000"
          ]

    infoMod :: O.InfoMod Options
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Chain seek server for Marlowe Runtime."
      , O.header "chainseekd : a chain seek server for the Marlowe Runtime."
      ]
