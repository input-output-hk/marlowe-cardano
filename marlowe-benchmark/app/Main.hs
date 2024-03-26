{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | Execute Benchmarks.
module Main (
  -- * Entry point
  main,
) where

import Cardano.Api (
  AsType (AsAddressAny, AsPaymentExtendedKey, AsSigningKey),
  File (File),
  deserialiseAddress,
  readFileTextEnvelope,
 )
import Control.Applicative (asum, (<|>))
import Control.Monad ((<=<))
import Data.Text (Text)
import Data.Version (showVersion)
import Data.Yaml (decodeFileEither)
import Language.Marlowe.Runtime.Benchmark (BenchmarkConfig, Faucet (..), LifecycleBenchmarkContext (..), measure)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressAny)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)
import Paths_marlowe_benchmark (version)

import qualified Cardano.Api as C
import qualified Language.Marlowe.Runtime.Benchmark.Query.Generate as Generate (queries)
import qualified Options.Applicative as O

data Command = Command
  { runtimeHostname :: String
  , runtimePort :: Int
  , eraOnwards :: EitherBabbageOrConwayEra
  , configFilePathMaybe :: Maybe FilePath
  , lifecycleBenchmarkConfigurationMaybe :: Maybe LifecycleBenchmarkConfiguration
  , outputFile :: Maybe FilePath
  }
  deriving (Show)

data FaucetConfiguration = FaucetConfiguration
  { address :: Text
  , privateKey :: FilePath
  }
  deriving (Show)

data LifecycleBenchmarkConfiguration = LifecycleBenchmarkConfiguration
  { node :: FilePath
  , networkId :: C.NetworkId
  , faucet :: FaucetConfiguration
  }
  deriving (Show)

newtype EitherBabbageOrConwayEra
  = EitherBabbageOrConwayEra (Either (C.BabbageEraOnwards C.BabbageEra) (C.BabbageEraOnwards C.ConwayEra))

instance Show EitherBabbageOrConwayEra where
  show (EitherBabbageOrConwayEra (Left _)) = "BabbageEra"
  show (EitherBabbageOrConwayEra (Right _)) = "ConwayEra"

-- | Execute the benchmarks.
main :: IO ()
main =
  do
    Command{..} <- O.execParser commandParser
    readingContext <- getReadingBenchmarkConfigurationOrDefault configFilePathMaybe
    lifecycleBenchmarkContextMaybe <- getLifecycleBenchmarkContext lifecycleBenchmarkConfigurationMaybe
    case eraOnwards of
      EitherBabbageOrConwayEra eraOnwards' ->
        case eraOnwards' of
          Left era -> pure (measure readingContext era lifecycleBenchmarkContextMaybe outputFile)
          Right era -> pure (measure readingContext era lifecycleBenchmarkContextMaybe outputFile)
      >>= connectToMarloweRuntime runtimeHostname (fromIntegral runtimePort)
  where
    getReadingBenchmarkConfigurationOrDefault :: Maybe FilePath -> IO BenchmarkConfig
    getReadingBenchmarkConfigurationOrDefault Nothing = pure Generate.queries
    getReadingBenchmarkConfigurationOrDefault (Just configFilePath) = either (error . show) id <$> decodeFileEither configFilePath

    getLifecycleBenchmarkContext
      :: Maybe LifecycleBenchmarkConfiguration
      -> IO (Maybe LifecycleBenchmarkContext)
    getLifecycleBenchmarkContext Nothing = pure Nothing
    getLifecycleBenchmarkContext (Just LifecycleBenchmarkConfiguration{faucet = faucetConfig, ..}) =
      do
        let nodeSocketPath = C.File node
        faucetContext <- getFaucetContext faucetConfig
        pure . Just $
          LifecycleBenchmarkContext
            { nodeSocketPath = nodeSocketPath
            , networkId = networkId
            , faucet = faucetContext
            }

getFaucetContext :: FaucetConfiguration -> IO Faucet
getFaucetContext FaucetConfiguration{..} =
  do
    address' <-
      maybe (error "Failed to parse faucet address.") (pure . fromCardanoAddressAny) $
        deserialiseAddress AsAddressAny address
    key' <-
      either (error . show) pure
        <=< readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)
        $ File privateKey
    pure Faucet{address = address', privateKey = key'}

commandParser :: O.ParserInfo Command
commandParser =
  O.info
    ( O.helper
        <*> showVersionP
        <*> commandP
    )
    ( O.fullDesc
        <> O.progDesc "This command-line tool executes benchmarks for Marlowe Runtime."
        <> O.header "marlowe-benchmark : execute Marlowe Runtime benchmarks"
    )
  where
    commandP :: O.Parser Command
    commandP =
      Command
        <$> runtimeHostnameP
        <*> runtimePortP
        <*> eraP
        <*> configFilePathMaybeP
        <*> lifecycleBenchmarkConfigurationMaybeP
        <*> outputFileP

    eraP :: O.Parser EitherBabbageOrConwayEra
    eraP =
      asum
        [ O.flag'
            (EitherBabbageOrConwayEra $ Left C.BabbageEraOnwardsBabbage)
            ( O.long "babbage-era"
                <> O.help "Specify the Babbage era (default)"
            )
        , O.flag'
            (EitherBabbageOrConwayEra $ Right C.BabbageEraOnwardsConway)
            ( O.long "conway-era"
                <> O.help "Specify the Conway era"
            )
        , pure (EitherBabbageOrConwayEra $ Left C.BabbageEraOnwardsBabbage)
        ]
    showVersionP :: O.Parser (a -> a)
    showVersionP = O.infoOption ("marlowe-benchmark " <> showVersion version) $ O.long "version" <> O.help "Show version"

    runtimeHostnameP :: O.Parser String
    runtimeHostnameP =
      O.strOption
        (O.long "host" <> O.value "localhost" <> O.showDefault <> O.metavar "HOST" <> O.help "Host for Marlowe proxy service.")

    runtimePortP :: O.Parser Int
    runtimePortP =
      O.option
        O.auto
        (O.long "port" <> O.value 3700 <> O.showDefault <> O.metavar "PORT" <> O.help "Port for Marlowe proxy service.")

    configFilePathMaybeP :: O.Parser (Maybe FilePath)
    configFilePathMaybeP = O.optional (O.strOption (O.long "config" <> O.metavar "FILE" <> O.help "Path to the benchmark configuration file."))

    lifecycleBenchmarkConfigurationMaybeP :: O.Parser (Maybe LifecycleBenchmarkConfiguration)
    lifecycleBenchmarkConfigurationMaybeP = O.optional $ LifecycleBenchmarkConfiguration <$> nodeP <*> magicP <*> faucetP
      where
        nodeP :: O.Parser FilePath
        nodeP = O.strOption (O.long "node-socket-path" <> O.metavar "FILE" <> O.help "Path to the Cardano node socket.")

        magicP :: O.Parser C.NetworkId
        magicP =
          O.flag' C.Mainnet (O.long "mainnet" <> O.help "Execute on the Cardano mainnet.")
            <|> ( C.Testnet . C.NetworkMagic . toEnum
                    <$> O.option O.auto (O.long "testnet-magic" <> O.metavar "INTEGER" <> O.help "Execute on a Cardano testnet.")
                )

        faucetP :: O.Parser FaucetConfiguration
        faucetP = FaucetConfiguration <$> addressP <*> privateKeyP
          where
            addressP :: O.Parser Text
            addressP = O.strOption (O.long "address" <> O.metavar "ADDRESS" <> O.help "Faucet address.")

            privateKeyP :: O.Parser FilePath
            privateKeyP = O.strOption (O.long "signing-key-file" <> O.metavar "FILE" <> O.help "Path to faucet signing key file.")

    outputFileP :: O.Parser (Maybe FilePath)
    outputFileP =
      O.optional
        (O.strOption (O.long "out-file" <> O.metavar "FILE" <> O.help "Path to the output file for benchmark results."))
