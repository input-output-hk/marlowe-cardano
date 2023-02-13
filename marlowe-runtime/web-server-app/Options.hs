{-# LANGUAGE StrictData #-}

module Options
  where

import qualified Language.Marlowe.Runtime.CLI.Option as O
import Network.Socket (HostName, PortNumber)
import Options.Applicative

data Options = Options
  { syncHost :: HostName
  , syncQueryPort :: PortNumber
  , txHost :: HostName
  , txCommandPort :: PortNumber
  , port :: PortNumber
  , openAPIEnabled :: Bool
  , accessControlAllowOriginAll :: Bool
  }

portOption :: O.CliOption OptionFields PortNumber
portOption = O.port "" "WEB" 8080 "The port number to serve the HTTP API on."

getOptions :: IO Options
getOptions = do
  syncHostParser <- O.optParserWithEnvDefault O.syncHost
  syncQueryPortParser <- O.optParserWithEnvDefault O.syncQueryPort
  txHostParser <- O.optParserWithEnvDefault O.txHost
  txCommandPortParser <- O.optParserWithEnvDefault O.txCommandPort
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
      <$> syncHostParser
      <*> syncQueryPortParser
      <*> txHostParser
      <*> txCommandPortParser
      <*> portParser
      <*> openAPIParser
      <*> accessControlAllowOriginAllParser
    infoMod = mconcat
      [ fullDesc
      , progDesc "Web server for the Marlowe Runtime REST API"
      ]
  execParser $ info (helper <*> parser) infoMod
