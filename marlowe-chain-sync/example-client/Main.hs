module Main where

import Control.Exception (bracket, bracketOnError, throwIO)
import Data.Functor (void)
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api (Move (..), TxOutRef (..), UTxOError (..), WithGenesis (..),
                                               runtimeChainSeekCodec, schemaVersion1_0)
import Network.Channel (socketAsChannel)
import Network.Protocol.ChainSeek.Client (ChainSeekClient (ChainSeekClient), ClientStHandshake (..), ClientStIdle (..),
                                          ClientStInit (..), ClientStNext (..), chainSeekClientPeer)
import Network.Protocol.Driver (mkDriver)
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
      connect sock $ addrAddress addr
      pure sock

-- | This example client skips every 1000 blocks until it catches up to the
-- tip, at which point it requests one block at a time.
run :: Socket -> IO ()
run conn = void $ runPeerWithDriver driver peer (startDState driver)
  where
    driver = mkDriver throwIO runtimeChainSeekCodec channel
    channel = socketAsChannel conn
    peer = chainSeekClientPeer Genesis client
    client = ChainSeekClient $ pure $ SendMsgRequestHandshake schemaVersion1_0 stHandshake
    stHandshake = ClientStHandshake
      { recvMsgHandshakeRejected = \supportedVersions -> do
          putStr "Schema version not supported by server. Supported versions: "
          print supportedVersions
      , recvMsgHandshakeConfirmed = stIdleGenesis
      }
    stIdleGenesis = pure $ SendMsgQueryNext (AdvanceSlots 25644) stNextGenesis (pure stNextGenesis)
    stNextGenesis = ClientStNext
      { recvMsgQueryRejected = absurd
      , recvMsgRollForward = \_ _ _ -> stIdleConsume
      , recvMsgRollBackward = \_ _ -> stIdleGenesis
      }
    stIdleConsume = pure $ SendMsgQueryNext (ConsumeUTxO (TxOutRef "59b32f725ef2909ed3af209d92f97a0a29198ed9b85b9a2ac3ca4bdd05817a53" 0)) stNext (pure stNext)
    stNext = ClientStNext
      { recvMsgQueryRejected = \case
          UTxONotFound -> error "UTxO not found"
          UTxOSpent _  -> error "UTxO already spent"
      , recvMsgRollForward = \tx _ _ -> do
          putStr "UTxO spent by transaction: "
          print tx
          pure $ SendMsgDone ()
      , recvMsgRollBackward = \point _ -> do
          putStr "Roll backward: "
          print point
          stIdleConsume
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
      , O.progDesc "Example Chain Seek client for the Marlowe Chain Sync"
      ]
