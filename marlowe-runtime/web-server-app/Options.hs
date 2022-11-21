{-# LANGUAGE StrictData #-}

module Options
  where

import qualified Language.Marlowe.Runtime.CLI.Option as O
import Network.Socket (HostName, PortNumber)
import Options.Applicative

data Options = Options
  { discoveryHost :: HostName
  , discoverySyncPort :: PortNumber
  , port :: PortNumber
  , openAPIEnabled :: Bool
  }

portOption :: O.CliOption OptionFields PortNumber
portOption = O.port "" "WEB" 8080 "The port number to serve the HTTP API on."

getOptions :: IO Options
getOptions = do
  discoveryHostParser <- O.optParserWithEnvDefault O.discoveryHost
  discoverySyncPortParser <- O.optParserWithEnvDefault O.discoverySyncPort
  portParser <- O.optParserWithEnvDefault portOption
  let
    openAPIParser = flag False True $ mconcat
      [ long "enable-open-api"
      , short 'o'
      , help "Serve the OpenAPI specification at /openapi.json"
      , showDefault
      ]
    parser = Options
      <$> discoveryHostParser
      <*> discoverySyncPortParser
      <*> portParser
      <*> openAPIParser
    infoMod = mconcat
      [ fullDesc
      , progDesc "Web server for the Marlowe Runtime REST API"
      ]
  execParser $ info (helper <*> parser) infoMod
