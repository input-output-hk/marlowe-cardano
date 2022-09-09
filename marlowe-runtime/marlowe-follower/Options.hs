module Options where

import Language.Marlowe.Runtime.Core.Api (ContractId (..), parseContractId)
import Network.Socket (HostName, PortNumber)
import Options.Applicative (argument, auto, execParser, fullDesc, header, help, helper, info, long, maybeReader,
                            metavar, option, progDesc, short, showDefault, strOption, value)

data Options = Options
  { port       :: PortNumber
  , queryPort  :: PortNumber
  , host       :: HostName
  , contractId :: ContractId
  }

getOptions :: IO Options
getOptions = execParser $ info (helper <*> parser) infoMod
  where
    parser = Options <$> portParser <*> queryPortParser <*> hostParser <*> contractIdParser

    portParser = option auto $ mconcat
      [ long "port-number"
      , value 3715
      , metavar "PORT_NUMBER"
      , help "The port number of the chain seek server."
      , showDefault
      ]

    queryPortParser = option auto $ mconcat
      [ long "query-port-number"
      , value 3716
      , metavar "PORT_NUMBER"
      , help "The query port number of the chain seek server."
      , showDefault
      ]

    hostParser = strOption $ mconcat
      [ long "host"
      , short 'h'
      , value "127.0.0.1"
      , metavar "HOST_NAME"
      , help "The host name of the chain seek server."
      , showDefault
      ]

    contractIdParser = argument (maybeReader parseContractId) $ mconcat
      [ metavar "CONTRACT_ID"
      , help "The UTxO that created the contract"
      ]

    infoMod = mconcat
      [ fullDesc
      , progDesc "Contract follower for Marlowe Runtime"
      , header "marlowe-follower : a contract follower for the Marlowe Runtime."
      ]
