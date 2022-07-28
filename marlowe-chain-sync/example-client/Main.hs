module Main where

import Cardano.Api (BlockHeader (BlockHeader), BlockNo (..), ChainPoint (..), ChainTip (..))
import Control.Exception (bracket, bracketOnError, throwIO)
import Data.Functor (void)
import Data.Void (absurd)
import Debug.Trace (traceShowId)
import Language.Marlowe.Runtime.ChainSync.Protocol (Query (..), runtimeFilteredChainSyncCodec, schemaVersion1_0)
import Network.Channel (socketAsChannel)
import Network.Protocol.Driver (mkDriver)
import Network.Protocol.FilteredChainSync.Client (ClientStHandshake (..), ClientStIdle (..), ClientStInit (..),
                                                  ClientStNext (..), FilteredChainSyncClient (FilteredChainSyncClient),
                                                  filteredChainSyncClientPeer)
import Network.Socket (AddrInfo (..), HostName, PortNumber, Socket, SocketType (..), close, connect, defaultHints,
                       getAddrInfo, openSocket)
import Network.TypedProtocol (Driver (..), runPeerWithDriver)
import qualified Options.Applicative as O

main :: IO ()
main = do
  Options{..} <- getOptions
  let hints = defaultHints { addrSocketType = Stream }
  addr <- head <$> getAddrInfo (Just hints) (Just host) (Just $ show port)
  bracket (open addr) close run
  where
    open addr = bracketOnError (openSocket addr) close \sock -> do
      connect sock $ traceShowId $ addrAddress addr
      pure sock

-- | This example client skips every 1000 blocks until it catches up to the
-- tip, at which point it requests one block at a time.
run :: Socket -> IO ()
run conn = void $ runPeerWithDriver driver peer (startDState driver)
  where
    driver = mkDriver throwIO runtimeFilteredChainSyncCodec channel
    channel = socketAsChannel conn
    peer = filteredChainSyncClientPeer ChainPointAtGenesis client
    client = FilteredChainSyncClient $ pure $ SendMsgRequestHandshake schemaVersion1_0 stHandshake
    stHandshake = ClientStHandshake
      { recvMsgHandshakeRejected = \supportedVersions -> do
          putStr "Schema version not supported by server. Supported versions: "
          print supportedVersions
      , recvMsgHandshakeConfirmed = stIdle 1
      }
    stIdle stepSize = do
      let query = WaitBlocks stepSize GetBlockHeader
      pure $ SendMsgQueryNext query stNext (pure stNext)
    stNext = ClientStNext
      { recvMsgQueryRejected = absurd
      , recvMsgRollForward = \(BlockHeader _ _ (BlockNo blockNo)) point tip -> do
          putStr "Roll forward: "
          print point
          stIdle $ fromIntegral case tip of
            ChainTipAtGenesis                 -> 1
            ChainTip _ _ (BlockNo tipBlockNo) -> max 1 (min 1000 $ tipBlockNo - blockNo)
      , recvMsgRollBackward = \point _ -> do
          putStr "Roll backward: "
          print point
          stIdle 1
      }

data Options = Options
  { port :: !PortNumber
  , host :: !HostName
  }

getOptions :: IO Options
getOptions = O.execParser $ O.info parser infoMod
  where
    parser = O.helper <*> (Options <$> portParser <*> hostParser)
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
    infoMod = mconcat
      [ O.fullDesc
      , O.progDesc "Example Filtered Chain Sync client for the Marlowe Chain Sync"
      ]
