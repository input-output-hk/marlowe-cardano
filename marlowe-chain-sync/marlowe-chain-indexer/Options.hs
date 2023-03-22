module Options
  ( Options(..)
  , getOptions
  ) where

import Cardano.Api (NetworkId(..), NetworkMagic(..))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy.Encoding (decodeUtf8)
import Language.Marlowe.Runtime.ChainIndexer.NodeClient (CostModel(..))
import Logging (defaultRootSelectorLogConfig)
import qualified Options.Applicative as O
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket        :: !FilePath
  , networkId         :: !NetworkId
  , databaseUri       :: !String
  , genesisConfigHash :: !Text
  , genesisConfigFile :: !FilePath
  , shelleyGenesisFile :: !FilePath
  , costModel         :: !CostModel
  , maxCost           :: !Int
  , logConfigFile     :: !(Maybe FilePath)
  , httpPort :: !Int
  } deriving (Show, Eq)

getOptions :: String -> IO Options
getOptions version = do
  defaultNetworkId <- O.value . fromMaybe Mainnet <$> readNetworkId
  defaultSocketPath <- maybe mempty O.value <$> readSocketPath
  defaultDatabaseUri <- maybe mempty O.value <$> readDatabaseUri
  O.execParser $ parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri version
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

parseOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.OptionFields String
  -> String
  -> O.ParserInfo Options
parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri version = O.info parser infoMod
  where
    parser :: O.Parser Options
    parser = O.helper
      <*> versionOption
      <*> printLogConfigOption
      <*> ( Options
              <$> socketPathOption
              <*> networkIdOption
              <*> databaseUriOption
              <*> genesisConfigHashOption
              <*> genesisConfigFileOption
              <*> shelleyGenesisFileOption
              <*> costModelParser
              <*> maxCostParser
              <*> logConfigFileParser
              <*> httpPortParser
          )
      where
        versionOption :: O.Parser (a -> a)
        versionOption = O.infoOption
          ("marlowe-chain-indexer " <> version)
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

        shelleyGenesisFileOption :: O.Parser String
        shelleyGenesisFileOption = O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options = mconcat
              [ O.long "shelley-genesis-config-file"
              , O.metavar "CONFIG_FILE"
              , O.help "Path to the Shelley Genesis Config JSON File."
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

        httpPortParser :: O.Parser Int
        httpPortParser = O.option O.auto options
          where
            options :: O.Mod O.OptionFields Int
            options = mconcat
              [ O.long "http-port"
              , O.metavar "PORT_NUMBER"
              , O.help "Port number to serve the http healthcheck API on"
              , O.value 8080
              , O.showDefault
              ]

        costModelParser :: O.Parser CostModel
        costModelParser = CostModel <$> blockCostParser <*> txCostParser

        blockCostParser :: O.Parser Int
        blockCostParser = O.option O.auto $ mconcat
          [ O.long "block-cost"
          , O.value 1
          , O.metavar "COST_UNITS"
          , O.help "The number of cost units to associate with persisting a block when computing the cost model."
          , O.showDefault
          ]

        txCostParser :: O.Parser Int
        txCostParser = O.option O.auto $ mconcat
          [ O.long "tx-cost"
          , O.value 10
          , O.metavar "COST_UNITS"
          , O.help "The number of cost units to associate with persisting a transaction when computing the cost model."
          , O.showDefault
          ]

        maxCostParser :: O.Parser Int
        maxCostParser = O.option O.auto $ mconcat
          [ O.long "max-cost"
          , O.value 100_000
          , O.metavar "COST_UNITS"
          , O.help "The maximum number of cost units that can be batched when persisting blocks. If the cost of the current batch would exceed this value, the chain sync client will wait until the current batch is persisted before requesting another block."
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

    infoMod :: O.InfoMod Options
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Chain indexer for Marlowe Runtime."
      , O.header "marlowe-chain-indexer : a chain indexer for the Marlowe Runtime."
      ]
