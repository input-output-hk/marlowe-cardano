module Options (
  Options (..),
  getOptions,
) where

import Cardano.Api (NetworkId (..), NetworkMagic (..))
import Data.Maybe (fromMaybe)
import qualified Options.Applicative as O
import Options.Applicative.Help.Pretty
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

data Options = Options
  { nodeSocket :: !FilePath
  , networkId :: !NetworkId
  , databaseUri :: !String
  }
  deriving (Show, Eq)

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
        _ -> value

    readDatabaseUri :: IO (Maybe String)
    readDatabaseUri = do
      value <- lookupEnv "CHAIN_SYNC_DB_URI"
      pure case value of
        Just "" -> Nothing
        _ -> value

parseOptions
  :: O.Mod O.OptionFields NetworkId
  -> O.Mod O.OptionFields FilePath
  -> O.Mod O.OptionFields String
  -> String
  -> O.ParserInfo Options
parseOptions defaultNetworkId defaultSocketPath defaultDatabaseUri version = O.info parser infoMod
  where
    parser :: O.Parser Options
    parser =
      O.helper
        <*> versionOption
        <*> ( Options
                <$> socketPathOption
                <*> networkIdOption
                <*> databaseUriOption
            )
      where
        versionOption :: O.Parser (a -> a)
        versionOption =
          O.infoOption
            ("marlowe-chain-copy " <> version)
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

    infoMod :: O.InfoMod Options
    infoMod =
      mconcat
        [ O.fullDesc
        , O.progDescDoc $ Just description
        , O.header "marlowe-chain-copy: A bulk data transfer utility for marlowe-chain-sync"
        ]

bullets :: [Doc] -> Doc
bullets = indent 2 . vcat . fmap (("•" <+>) . align)

description :: Doc
description =
  concatWith
    (\a b -> a <> line <> line <> b)
    [ vcat
        [ "marlowe-chain-copy is a admin utility designed to speed up initial synchronization"
        , "of data from a fully caught-up cardano-node into a new marlowe-chain-sync database."
        , "It offers roughly double the throughput of marlowe-chain-indexer during initial sync"
        , "by performing the following optimizations:"
        ]
    , bullets
        [ "Truncating all tables before the copy begins"
        , "Disabling index updates during sync"
        , "Streaming data to all tables in parallel using COPY operations"
        , "Enabling indexes and re-indexing all tables after sync completes"
        ]
    , vcat
        [ "Because of these optimizations, this tool is not meant to be used during normal"
        , "Runtime operations. Instead, it should be used as an ops tool for provisioning new"
        , "Runtime instances."
        ]
    , vcat
        [ "CAUTION: because of the parallel streaming writes and the initial truncation of"
        , "tables, once marlowe-chain-copy starts, it must be allowed to run to completion"
        , "before the database is fit for use by marlowe-chain-sync and marlowe-chain-indexer."
        , "Early termination of the process will result in the transactions initiated by the"
        , "COPY operations being rolled back, and the tables will be left truncated."
        ]
    , vcat
        [ "Running against a fully synchronized node is necessary for realizing performance"
        , "benefits, as node sync rate will become the primary bottleneck. If operating against"
        , "a new node that is still synchronizing, using marlowe-chain-copy offers little benefit"
        , "compared with simply running marlowe-chain-indexer."
        ]
    ]
