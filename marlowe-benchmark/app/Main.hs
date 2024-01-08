-- | Execute Benchmarks.
module Main (
  -- * Entry point
  main,
) where

import Data.Aeson (eitherDecodeFileStrict')
import Data.Default (def)
import Data.Version (showVersion)
import Language.Marlowe.Runtime.Benchmark (measure)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntime)
import Paths_marlowe_benchmark (version)

import qualified Options.Applicative as O

-- | Execute the benchmarks.
main :: IO ()
main =
  do
    (host, port, config) <- O.execParser commandParser
    config' <-
      case config of
        Nothing -> pure def
        Just config'' -> either error id <$> eitherDecodeFileStrict' config''
    connectToMarloweRuntime host (fromIntegral port) $ measure config'

commandParser :: O.ParserInfo (String, Int, Maybe FilePath)
commandParser =
  let commandOptions =
        (,,)
          <$> O.strOption (O.long "host" <> O.value "localhost" <> O.metavar "HOST" <> O.help "Host for Marlowe proxy service.")
          <*> O.option O.auto (O.long "port" <> O.value 3700 <> O.metavar "PORT" <> O.help "Port for Marlowe proxy service.")
          <*> (O.optional . O.strOption) (O.long "config" <> O.metavar "FILE" <> O.help "Path to the benchmark configuration file.")
   in O.info
        (O.helper <*> (O.infoOption (showVersion version) $ O.long "version" <> O.help "Show version") <*> commandOptions)
        ( O.fullDesc
            <> O.progDesc "This command-line tool executes benchmarks for Marlowe Runtime."
            <> O.header "marlowe-benchmark : execute Marlowe Runtime benchmarks"
        )
