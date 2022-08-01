module Options
  ( Options(..)
  , getOptions
  ) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Options.Applicative as O
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket        :: !FilePath
  , networkId         :: !NetworkId
  , databaseUri       :: !String
  , genesisConfigHash :: !Text
  , genesisConfigFile :: !FilePath
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
      <*> ( Options
              <$> socketPathOption
              <*> networkIdOption
              <*> databaseUriOption
              <*> genesisConfigHashOption
              <*> genesisConfigFileOption
          )
      where
        versionOption :: O.Parser (a -> a)
        versionOption = O.infoOption
          ("marlowesyncd " <> version)
          (O.long "version" <> O.help "Show version.")

        socketPathOption :: O.Parser FilePath
        socketPathOption = O.strOption options
          where
            options :: O.Mod O.OptionFields FilePath
            options = mconcat
              [ O.long "socket-path"
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
              , O.metavar "INTEGER"
              , defaultNetworkId
              , O.help "Testnet network ID magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable."
              ]

    infoMod :: O.InfoMod Options
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Chain sync client for Marlowe Runtime."
      , O.header "marlowesyncd : a chain sync client for the Marlowe Runtime."
      ]
