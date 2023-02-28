{-# LANGUAGE StrictData #-}

module Options
  where

import qualified Language.Marlowe.Runtime.CLI.Option as O
import Network.Socket (HostName, PortNumber)
import Options.Applicative

data Options = Options
  { runtimeHost :: HostName
  , runtimePort :: PortNumber
  , port :: PortNumber
  , openAPIEnabled :: Bool
  , accessControlAllowOriginAll :: Bool
  }

portOption :: O.CliOption OptionFields PortNumber
portOption = O.port "http" "WEB" 8080 "The port number to serve the HTTP API on."

getOptions :: IO Options
getOptions = do
  runtimeHostParser <- O.optParserWithEnvDefault O.runtimeHost
  runtimePortParser <- O.optParserWithEnvDefault O.runtimePort
  portParser <- O.optParserWithEnvDefault portOption
  let
    openAPIParser = flag False True $ mconcat
      [ long "enable-open-api"
      , short 'o'
      , help "Serve the OpenAPI specification at /openapi.json"
      , showDefault
      ]
    accessControlAllowOriginAllParser = flag False True $ mconcat
      [ long "access-control-allow-origin-all"
      , short 'a'
      , help "Add 'Access-Control-Allow-Origin: *' header"
      , showDefault
      ]
    parser = Options
      <$> runtimeHostParser
      <*> runtimePortParser
      <*> portParser
      <*> openAPIParser
      <*> accessControlAllowOriginAllParser
    infoMod = mconcat
      [ fullDesc
      , progDesc "Web server for the Marlowe Runtime REST API"
      ]
  execParser $ info (helper <*> parser) infoMod
