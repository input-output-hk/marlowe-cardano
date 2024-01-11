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
import Control.Monad ((<=<))
import Data.Aeson (eitherDecodeFileStrict')
import Data.Default (def)
import Data.Text (Text)
import Data.Version (showVersion)
import Data.Word (Word32)
import Language.Marlowe.Runtime.Benchmark (measure)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoAddressAny)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)
import Paths_marlowe_benchmark (version)

import qualified Cardano.Api as C
import qualified Options.Applicative as O

-- | Execute the benchmarks.
main :: IO ()
main =
  do
    Command{..} <- O.execParser commandParser
    config' <-
      case config of
        Nothing -> pure def
        Just config'' -> either error id <$> eitherDecodeFileStrict' config''
    faucet' <-
      case faucet of
        Just Faucet{..} ->
          do
            address' <-
              maybe (error "Failed to parse faucet address.") (pure . fromCardanoAddressAny) $
                deserialiseAddress AsAddressAny address
            key' <-
              either (error . show) pure
                <=< readFileTextEnvelope (AsSigningKey AsPaymentExtendedKey)
                $ File key
            pure $ Just (C.File node, C.BabbageEra, C.Testnet $ C.NetworkMagic magic, address', key')
        Nothing -> pure Nothing
    connectToMarloweRuntime host (fromIntegral port) $ measure config' faucet' out

data Command = Command
  { host :: String
  , port :: Int
  , config :: Maybe FilePath
  , faucet :: Maybe Faucet
  , out :: Maybe FilePath
  }
  deriving (Show)

data Faucet = Faucet
  { node :: FilePath
  , magic :: Word32
  , address :: Text
  , key :: FilePath
  }
  deriving (Show)

commandParser :: O.ParserInfo Command
commandParser =
  let commandOptions =
        Command
          <$> O.strOption (O.long "host" <> O.value "localhost" <> O.metavar "HOST" <> O.help "Host for Marlowe proxy service.")
          <*> O.option O.auto (O.long "port" <> O.value 3700 <> O.metavar "PORT" <> O.help "Port for Marlowe proxy service.")
          <*> (O.optional . O.strOption) (O.long "config" <> O.metavar "FILE" <> O.help "Path to the benchmark configuration file.")
          <*> ( O.optional $
                  Faucet
                    <$> O.strOption (O.long "node-socket-path" <> O.metavar "FILE" <> O.help "Path to the Cardano node socket.")
                    <*> O.option O.auto (O.long "network-magic" <> O.metavar "INTEGER" <> O.help "The Cardano network magic number.")
                    <*> O.strOption (O.long "address" <> O.metavar "ADDRESS" <> O.help "Faucet address.")
                    <*> O.strOption (O.long "signing-key-file" <> O.metavar "FILE" <> O.help "Path to faucet signing key file.")
              )
          <*> (O.optional . O.strOption)
            (O.long "out-file" <> O.metavar "FILE" <> O.help "Path to the output file for benchmark results.")
   in O.info
        (O.helper <*> (O.infoOption (showVersion version) $ O.long "version" <> O.help "Show version") <*> commandOptions)
        ( O.fullDesc
            <> O.progDesc "This command-line tool executes benchmarks for Marlowe Runtime."
            <> O.header "marlowe-benchmark : execute Marlowe Runtime benchmarks"
        )
