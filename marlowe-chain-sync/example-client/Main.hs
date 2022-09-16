module Main
  where

import Control.Exception (bracket, bracketOnError, throwIO)
import Data.Functor (void)
import qualified FollowingUTxOs
import Language.Marlowe.Runtime.ChainSync.Api (WithGenesis(..), runtimeChainSeekCodec)
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (chainSeekClientPeer)
import Network.Protocol.Driver (mkDriver)
import Network.Socket
  (AddrInfo(..), HostName, PortNumber, Socket, SocketType(..), close, connect, defaultHints, getAddrInfo, openSocket)
import Network.TypedProtocol (Driver(..), runPeerWithDriver)
import Numeric.Natural (Natural)
import Options.Applicative (auto)
import qualified Options.Applicative as O
import qualified SkippingBlocks

main :: IO ()
main = do
  Options{..} <- getOptions
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
  bracket (open addr) close (run example)
  where
    open addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ addrAddress addr
      pure sock

run :: Natural -> Socket -> IO ()
run example conn = void $ runPeerWithDriver driver peer (startDState driver)
  where
    driver = mkDriver throwIO runtimeChainSeekCodec channel
    channel = socketAsChannel conn
    peer = chainSeekClientPeer Genesis client
    client = case example of
      0 -> SkippingBlocks.client
      1 -> FollowingUTxOs.client
      _ -> error "Invalid example"

data Options = Options
  { port    :: !PortNumber
  , host    :: !HostName
  , example :: !Natural
  }

getOptions :: IO Options
getOptions = O.execParser $ O.info parser infoMod
  where
    parser = O.helper <*> (Options <$> portParser <*> hostParser <*> exampleParser)
    portParser = O.option O.auto $ mconcat
      [ O.long "port-number"
      , O.short 'p'
      , O.value 3715
      , O.metavar "PORT_NUMBER"
      , O.help "The port number of the chain sync server to connect to"
      ]
    hostParser = O.strOption $ mconcat
      [ O.long "host"
      , O.short 'h'
      , O.value "127.0.0.1"
      , O.metavar "HOST_NAME"
      , O.help "The hostname of the chain sync server to connect to"
      ]
    exampleParser = O.argument auto $ mconcat
      [ O.metavar "EXAMPLE_NUM"
      , O.help "The example to run: 0: skipping blocks, 1: following UTxOs"
      ]
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Example Chain Seek client for the Marlowe Chain Sync"
      ]
