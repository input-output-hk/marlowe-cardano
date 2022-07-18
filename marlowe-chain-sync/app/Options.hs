module Options
  ( Options(..)
  , getOptions
  ) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Data.Maybe (fromMaybe)
import qualified Options.Applicative as O
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket :: !FilePath
  , networkId  :: !NetworkId
  } deriving (Show, Eq)

getOptions :: String -> IO Options
getOptions version = do
  defaultNetworkId <- O.value . fromMaybe Mainnet <$> readNetworkId
  defaultSocketPath <- maybe mempty O.value <$> readSocketPath
  O.execParser $ parseOptions defaultNetworkId defaultSocketPath version
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

parseOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> String
  -> O.ParserInfo Options
parseOptions defaultNetworkId defaultSocketPath version = O.info parser infoMod
  where
    parser :: O.Parser Options
    parser = O.helper <*> versionOption <*> (Options <$> socketPathOption <*> networkIdOption)
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
